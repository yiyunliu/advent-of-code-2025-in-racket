#lang racket
(require racket/generator)

(define (parse-from-port port)
  (for ([line (sequence-map
               string-trim
               (in-lines port))]
        #:break (string=? line ""))
    (define result (regexp-match #px"(\\d+),(\\d+)" line))
    (yield (cons (string->number (second result))
                 (string->number (third result))))))

(define (main port)
  (define points
    (for/vector ([point (in-generator (parse-from-port port))])
      point))
  (define size (vector-length points))
  (for*/fold
      ([max-area 0])
      ([i (in-range (sub1 size))]
       [j (in-range (add1 i) size)])
    (match-define (cons x0 y0) (vector-ref points i))
    (match-define (cons x1 y1) (vector-ref points j))
    (max max-area (* (add1 (abs (- x0 x1))) (add1 (abs (- y0 y1)))))))


;; - lines (same y coord)
;; x1 >= x0
(struct Horizontal (y x0 x1) #:transparent)
;; | lines (same x coord)
;; y0 >= y1
(struct Vertical (x y0 y1) #:transparent)


(define (points->line xy0 xy1)
  (match-define (cons x0 y0) xy0)
  (match-define (cons x1 y1) xy1)
  (cond
    ([eqv? x0 x1]
     (Vertical x0 (min y0 y1) (max y0 y1)))
    (else
     (Horizontal y0 (min x0 x1) (max x0 x1)))))

(define (tiles->lines tiles)
  (for/fold
      ([last-tile
        (vector-ref tiles (sub1 (vector-length tiles)))]
       [lines
        '()])
      ([tile tiles])
    (values tile (cons (points->line tile last-tile) lines))))
