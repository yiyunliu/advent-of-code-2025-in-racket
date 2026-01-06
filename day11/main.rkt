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


(define (main filename)
  (define graph (call-with-input-file filename parse-from-port))
  (define npaths (make-hash '((out . 1))))
  (dfs graph 'you npaths)
  (hash-ref npaths 'you))
