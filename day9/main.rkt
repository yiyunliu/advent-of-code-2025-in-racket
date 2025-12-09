#lang racket
(require racket/generator)

(define (parse-from-port port)
  (generator
   ()
   (for ([line (sequence-map
                string-trim
                (in-lines port))]
         #:break (string=? line ""))
     (define result (regexp-match #px"(\\d+),(\\d+)" line))
     (yield (cons (string->number (second result))
                  (string->number (third result)))))))

(define (main port)
  (define points
    (for/vector ([point (in-producer (parse-from-port port) (void))])
      point))
  (define size (vector-length points))
  (for*/fold
      ([max-area 0])
      ([i (in-range (sub1 size))]
       [j (in-range (add1 i) size)])
    (match-define (cons x0 y0) (vector-ref points i))
    (match-define (cons x1 y1) (vector-ref points j))
    (max max-area (* (add1 (abs (- x0 x1))) (add1 (abs (- y0 y1)))))))
