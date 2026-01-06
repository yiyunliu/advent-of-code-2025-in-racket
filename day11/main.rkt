#lang racket

(define (parse-from-port port)
  (for/hash ([line (sequence-map string-trim (in-lines port))]
        #:unless (string=? line ""))
    (define result (regexp-match #rx"(.*): (.*)" line))
    (define src (string->symbol (second result)))
    (define tgts (map string->symbol (string-split (third result))))
    (values src tgts)))


;; invariant: npaths[src] is not true
(define (dfs graph src npaths)
  (define total
    (for/fold
      ([acc 0])
      ([tgt (hash-ref graph src)])
      (unless (hash-has-key? npaths tgt)
        (dfs graph tgt npaths))
      (+ acc (hash-ref npaths tgt))))
  (hash-set! npaths src total))

(define (npaths-between graph src tgt)
  (define npaths (make-hash (list (cons 'out 0))))
  (hash-set! npaths tgt 1)
  (dfs graph src npaths)
  (hash-ref npaths src))

(define (main filename)
  (define graph (call-with-input-file filename parse-from-port))
  (npaths-between graph 'you 'out))

(define (main-p2 filename)
  (define graph (call-with-input-file filename parse-from-port))
  (+ (* (npaths-between graph 'svr 'dac)
        (npaths-between graph 'dac 'fft)
        (npaths-between graph 'fft 'out))
     (* (npaths-between graph 'svr 'fft)
        (npaths-between graph 'fft 'dac)
        (npaths-between graph 'dac 'out))))

(provide main main-p2)
