#lang racket
(require racket/generator)

(define (number->digits n)
  (generator
   ()
   (let loop ([n n])
     (when (positive? n)
       (define-values (q r) (quotient/remainder n 10))
       (yield r)
       (loop q)))))


(define (invalid? n)
  (define digits
    (for/vector
      ([digit (in-producer (number->digits n) (void))])
      digit))
  (for/or
      ([len (in-inclusive-range 1 (quotient (vector-length digits) 2))]
       #:when (zero? (remainder (vector-length digits) len)))
    (define rep (vector-take digits len))
    (for/and
        ([i (in-range (vector-length digits))])
      (= (vector-ref digits i)
         (vector-ref rep (remainder i len))))))

(define (find-invalid-in-range lo hi)
  (generator
   ()
   (let loop ([lo lo])
     (when (<= lo hi)
       (when (invalid? lo)
         (yield lo))
       (loop (add1 lo))))))

(define (main port)
  (for/fold
        ([acc 0])
        ([lohi (string-split (port->string port) ",")])
    (match (string-split lohi "-")
      [(list lo hi)
       (for/fold
           ([acc acc])
           ([inv-num (in-producer
                      (find-invalid-in-range (string->number lo)
                                             (string->number (string-trim hi)))
                      (void))])
         (+ acc inv-num))])))


(define (main-from-file filename)
  (call-with-input-file filename
    (lambda (port) (main port))))
