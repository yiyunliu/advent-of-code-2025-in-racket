#lang racket
(require racket/generator)
(require rackunit)

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
  (call-with-values
   (λ ()
     (for/fold
      ([last-tile
        (vector-ref tiles (sub1 (vector-length tiles)))]
       [lines
        '()])
      ([tile tiles])
       (values tile (cons (points->line tile last-tile) lines))))
   (lambda (_ y) y)))

(define/contract (points->borders xy0 xy1)
  (-> (cons/c number? number?) (cons/c number? number?) (listof (or/c Horizontal? Vertical?)))
  (match-define (cons _x0 _y0) xy0)
  (match-define (cons _x1 _y1) xy1)
  (define x0 (min _x0 _x1))
  (define x1 (max _x0 _x1))
  (define y0 (min _y0 _y1))
  (define y1 (max _y0 _y1))
  (list
   ;; left
   (Vertical x0 y0 y1)
   ;; right
   (Vertical x1 y0 y1)
   ;; down
   (Horizontal y0 x0 x1)
   ;; up
   (Horizontal y1 x0 x1)))


(define (left-ints left-line vers)
  (define x (Vertical-x left-line))
  (define lint (line->interval left-line))
  (for/fold
      ([acc '()])
      ([ver vers]
       #:do [(define int (line->interval ver))]
       #:when (and (<= (Vertical-x ver) x) (overlapping? int lint)))
    (cons int acc)))

(define (right-ints right-line vers)
  (define x (Vertical-x right-line))
  (define lint (line->interval right-line))
  (for/fold
      ([acc '()])
      ([ver vers]
       #:do [(define int (line->interval ver))]
       #:when (and (>= (Vertical-x ver) x) (overlapping? int lint)))
    (cons int acc)))

(define (up-ints up-line hors)
  (define y (Horizontal-y up-line))
  (define uint (line->interval up-line))
  (for/fold
      ([acc '()])
      ([ver hors]
       #:do [(define int (line->interval ver))]
       #:when (and (>= (Horizontal-y ver) y) (overlapping? int uint)))
    (cons int acc)))

(define (down-ints down-line hors)
  (define y (Horizontal-y down-line))
  (define uint (line->interval down-line))
  (for/fold
      ([acc '()])
      ([ver hors]
       #:do [(define int (line->interval ver))]
       #:when (and (<= (Horizontal-y ver) y) (overlapping? int uint)))
    (cons int acc)))

(define/contract (partition-lines lines)
  (-> (listof (or/c Horizontal? Vertical?))
      (values (listof Horizontal?) (listof Vertical?)))
  (partition Horizontal? lines))

(define (overlapping? i0 i1)
  (match-define (cons x0 y0) i0)
  (match-define (cons x1 y1) i1)
  (or (and (<= x0 x1) (<= x1 y0))
      (and (<= x1 x0) (<= x0 y1))))

(define (line->interval l)
  (match l
    [(Vertical _ y0 y1) (cons y0 y1)]
    [(Horizontal _ x0 x1) (cons x0 x1)]))

(define (fully-covered? xy ints)
  (match-define (cons x y) xy)
  (define sorted-ints (sort ints < #:key car))
  (cond
    [(null? sorted-ints) #f]
    [else
     (match-define (cons (cons start end) sorted-ints0) sorted-ints)
     (cond
       [(> start x) #f]
       [else
        (let loop ([end end] [sorted-ints0 sorted-ints0])
          (cond
            [(>= end y) #t]
            [(null? sorted-ints0) #f]
            [else
             (match-define (cons (cons x0 y0) sorted-ints1) sorted-ints0)
             (if (> x0 end) #f (loop (max y0 end) sorted-ints1))]))])]))


(define (main-p2 port)
  (define points
    (for/vector ([point (in-generator (parse-from-port port))])
      point))
  (define-values (hors vers) (partition-lines (tiles->lines points)))
  (define size (vector-length points))
  (for*/fold
      ([max-area 0])
      ([i (in-range (sub1 size))]
       [j (in-range (add1 i) size)])

    (match-define xy0 (vector-ref points i))
    (match-define xy1 (vector-ref points j))

    (match-define (list left right down up) (points->borders xy0 xy1))

    (define lints (left-ints left vers))
    (define rints (right-ints right vers))
    (define dints (down-ints down hors))
    (define uints (up-ints up hors))
    (cond
      [(and (fully-covered? (line->interval left) lints)
            (fully-covered? (line->interval right) rints)
            (fully-covered? (line->interval up) uints)
            (fully-covered? (line->interval down) dints))
       (max max-area (* (add1 (abs (- (car xy0) (car xy1)))) (add1 (abs (- (cdr xy0) (cdr xy1))))))]
      [else
       max-area])))

(module+ test
  (check-equal?
   (points->line '(0 . 1) '(0 . 9))
   (Vertical 0 1 9))

  (check-equal?
   (points->line '(0 . 9) '(0 . 1))
   (Vertical 0 1 9))

  (check-equal?
   (points->line '(3 . 4) '(9 . 4))
   (Horizontal 4 3 9))

  (check-true
   (fully-covered? '(0 . 100) '((60 . 61) (0 . 50) (50 . 60) (61 . 100))))

  (check-false
   (fully-covered? '(0 . 100) '((0 . 50) (50 . 60) (61 . 100)))))
