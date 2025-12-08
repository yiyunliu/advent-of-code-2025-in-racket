#lang racket
(require racket/generator)

(define (convert-number-or-operator str)
  (match str
    ["*" *]
    ["+" +]
    [_ (string->number str)]))

;; part 1

(define (transpose matrix)
  (generator
   ()
   (for ([i (in-range (vector-length (vector-ref matrix 0)))])
     (yield
      (for/list
        ([j (in-range (vector-length matrix))])
        (vector-ref (vector-ref matrix (- (vector-length matrix) j 1)) i))))))

(define (parse-matrix p)
  (for/vector ([line (sequence-map string-trim (in-lines p))])
    #:break (string=? line "")
    (list->vector (map convert-number-or-operator (string-split line #px"\\s+")))))

(define (main p)
  (define matrix (parse-matrix p))
  (for/sum
      ([op-args (in-producer (transpose matrix) (void))])
    (apply (car op-args) (cdr op-args))))

;; part 2

(define (parse-matrix-p2 p)
  (for/vector ([line (in-lines p)])
    #:break (string=? line "")
    (build-vector (string-length line) (lambda (i) (string-ref line i)))))


(define (matrix-ref matrix row col)
  (if (and (<= 0 row)
       (< row (vector-length matrix))
       (<= 0 col)
       (< col (vector-length (vector-ref matrix row))))
      (vector-ref (vector-ref matrix row) col)
      #\space))

(define (build-screen matrix)
  (define screen-width (sub1 (vector-length matrix)))
  (define screen-height (apply max (for/list
                                       ([row matrix])
                                     (vector-length row))))
  (build-vector
   screen-height
   (lambda (i)
     (build-string
      screen-width
      (lambda (j)
        (matrix-ref matrix j i))))))

(define (extract-ops matrix)
  (define last-row (vector-ref matrix (sub1 (vector-length matrix))))
  (generator
   ()
   (for ([op
          (string-split
           (build-string (vector-length last-row)
                         (lambda (k) (vector-ref last-row k)))
           #px"\\s+")])
     (yield (convert-number-or-operator op)))))

(define (process-numbers screen)
  (generator
   ()
   (for ([line (sequence-map string-trim screen)])
     (cond
       [(string=? line "")
        (yield 'compute)]
       [else
        (yield (string->number line))]))
   (yield 'compute)))

(define (render-screen matrix)
  (for ([line matrix])
    (displayln line)))

(define (main-p2 port)
  (define matrix (parse-matrix-p2 port))
  (define screen (build-screen matrix))
  (define ops (extract-ops matrix))
  (define numbers (process-numbers screen))
  (for/sum ([op (in-producer ops (void))])
    (define args (for/list ([number (in-producer numbers 'compute)]) number))
    ;; (printf "applying ~a to ~a\n" op args)
    (apply op args)))

(provide main main-p2 render-screen)
