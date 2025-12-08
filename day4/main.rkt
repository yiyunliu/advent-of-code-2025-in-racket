#lang racket

(require racket/splicing)
(require racket/generator)

(define (parse-text port)
  (for/vector
      ([line (sequence-map string-trim (in-lines port))]
       #:when (not (string=? (string-trim line) "")))
    (build-vector (string-length line) (lambda (i) (string-ref line i)))))

(define (matrix-dim matrix)
  (values (vector-length matrix) (vector-length (vector-ref matrix 0))))

(define (matrix-ref matrix coord)
  (vector-ref (vector-ref matrix (car coord)) (cdr coord)))

(define (matrix-remove! matrix coord)
  (vector-set! (vector-ref matrix (car coord)) (cdr coord) #\.))


(define (coord-valid? matrix coord)
  (match-define (cons i j) coord)
  (define-values (nrows ncols) (matrix-dim matrix))
  (and (>= i 0) (>= j 0) (< i nrows) (< j ncols)))

(splicing-let
    ([coord-add (lambda (coord0 coord1) (cons (+ (car coord0) (car coord1))
                                              (+ (cdr coord0) (cdr coord1))))]
     [dirs '#((1 . 0) (1 . 1) (0 . 1) (-1 . 0) (-1 . -1) (0 . -1) (1 . -1) (-1 . 1))])
  (define (neighbors matrix coord)
    (generator
     ()
     (for ([dir dirs])
      (define new-coord (coord-add coord dir))
      (when (coord-valid? matrix new-coord)
        (yield new-coord))))))

(define (matrix-paper? matrix coord)
  (char=? (matrix-ref matrix coord) #\@))

;; factored out from part 1
(define (gen-removable matrix)
  (define-values (nrows ncols) (matrix-dim matrix))
  (generator ()
   (for*
      ([i (in-range nrows)]
       [j (in-range ncols)]
       #:do [(define coord (cons i j))]
       #:when (matrix-paper? matrix coord))
    (define num-neigh
      (for/sum ([coord (in-producer (neighbors matrix coord) (void))])
        (if (matrix-paper? matrix coord) 1 0)))
    (when (< num-neigh 4)
      (yield coord)))))

;; part 1
(define (main port)
  (define matrix (parse-text port))
  (for/sum ([_ (in-producer (gen-removable matrix) (void))]) 1))

;; part 2
(define (main-part2 port)
  (define matrix (parse-text port))
  (define removed-count
    (generator
     ()
     (let loop ()
       (define removed
         (for/sum
             ([coord (in-producer (gen-removable matrix) (void))])
           (matrix-remove! matrix coord)
           1))
       (when (> removed 0)
         (yield removed)
         (loop)))))
  (for/sum ([r (in-producer removed-count (void))])
    r))

(provide main main-part2)
