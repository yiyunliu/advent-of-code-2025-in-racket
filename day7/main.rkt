#lang racket
(require racket/generator)

(define (parse-beams p)
  (generator
   ()
   (for ([line (in-lines p)])
     #:break (string=? line "")
     (yield line))))

;; part 1
(define (main p)
  (define gen (parse-beams p))
  (define init-set
    (set (string-find (gen) "S")))
  (for/fold
      ([beams init-set]
       [num-splits 0])
      ([line (in-producer gen (void))])
    (for/fold
        ([beams (set)]
         [num-splits num-splits])
        ([beam beams])
      (if (char=? (string-ref line beam) #\^)
          (values (set-union
                   beams
                   (list->set
                    (filter (lambda (x) (>= x 0) (< x (string-length line)))
                            (list (sub1 beam) (add1 beam)))))
                  (add1 num-splits))
          (values (set-add beams beam)
                  num-splits)))))

;; part 2

(define (main-part2 p)
  (define gen (parse-beams p))
  (define init-vec
    (let* ([line (gen)]
           [result (make-vector (string-length line) 0)])
      (vector-set! result (string-find line "S") 1)
      result))
  (define vec
    (for/fold
      ([beams init-vec])
      ([line (in-producer gen (void))])
      (define new-beams (make-vector (string-length line) 0))
      (for ([(cnt idx) (in-indexed beams)]
            #:unless (zero? cnt))
        (if (char=? (string-ref line idx) #\^)
            (for ([idx (filter (lambda (x) (>= x 0) (< x (string-length line)))
                               (list (sub1 idx) (add1 idx)))])
              (vector-set! new-beams idx (+ (vector-ref new-beams idx) cnt)))
            (vector-set! new-beams idx (+ (vector-ref new-beams idx) cnt))))
      new-beams))
  (for/sum ([num vec]) num))

(provide main main-part2)
