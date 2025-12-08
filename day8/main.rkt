#lang racket
(require racket/generator)
(require data/union-find)

(define (parse-line port)
  (generator
   ()
   (for ([line (sequence-map string-trim (in-lines port))]
         #:break (string=? line ""))
     (define result (regexp-match #px"(\\d+),(\\d+),(\\d+)" line))
     (yield (list->vector (map string->number (cdr result)))))))


(define (distance p0 p1)
  (for/sum
      ([v0 p0]
       [v1 p1])
    (expt (- v0 v1) 2)))


(define (port->points port)
  (for/vector ([p (in-producer (parse-line port) (void))]) p))

;; use vector index as ids

(define (all-pairs n)
  (generator () (for* ([i (in-range (sub1 n))] [j (in-range (add1 i) n)]) (yield (cons i j)))))


;; part 1
(define (main port cnt)
  (define points (port->points port))
  (define size (vector-length points))
  (define reps (build-vector size (λ (i) (uf-new i))))
  (define pairs
    (for/vector
        ([pair (in-producer (all-pairs size) (void))])
      pair))
  (vector-sort! pairs < #:key (λ (i) (distance
                                       (vector-ref points (car i))
                                       (vector-ref points (cdr i))))
                #:cache-keys? #t)
  (for ([_ (in-range cnt)] [pair pairs])
    (match-define (cons i j) pair)
    (define iset (vector-ref reps i))
    (define jset (vector-ref reps j))
    (unless (uf-same-set? iset jset)
      (uf-union! iset jset)))
  (define count (make-vector size 0))
  (for ([rep reps])
    (define rep-idx (uf-find rep))
    (vector-set! count rep-idx (add1 (vector-ref count rep-idx))))
  (vector-sort! count >)
  ;; sure a heap has better time complexity but sorting is quite fast
  (for/product
      ([i (in-range 3)])
    (vector-ref count i)))

(define (main-p2 port)
  (define points (port->points port))
  (define size (vector-length points))
  (define reps (build-vector size (λ (i) (uf-new i))))
  (define pairs
    (for/vector
        ([pair (in-producer (all-pairs size) (void))])
      pair))
  (vector-sort! pairs < #:key (λ (i) (distance
                                       (vector-ref points (car i))
                                       (vector-ref points (cdr i))))
                #:cache-keys? #t)
  ;; awful imperative programming
  (define count (make-vector size 1))
  (let/cc return
    (for ([pair pairs])
      (match-define (cons i j) pair)
      (define iset (vector-ref reps i))
      (define jset (vector-ref reps j))
      (define i-canon (uf-find iset))
      (define j-canon (uf-find jset))
      (unless (= i-canon j-canon)
        (define i-count (vector-ref count i-canon))
        (define j-count (vector-ref count j-canon))
        (define new-count (+  i-count j-count))
        (when (= new-count size)
          (return (* (vector-ref (vector-ref points i) 0)
                     (vector-ref (vector-ref points j) 0))))
        (vector-set! count i-canon 0)
        (vector-set! count j-canon 0)
        (uf-union! iset jset)
        (vector-set! count (uf-find iset) new-count)))))

(provide main main-p2)
