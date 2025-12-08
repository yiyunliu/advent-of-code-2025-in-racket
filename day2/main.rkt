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


(define (number->prefix n)
  (define-values (len digits)
    (for/fold ([len 0]
               [digits '()])
              ([d (in-producer (number->digits n) (void))])
      (values (add1 len) (cons d digits))))
  (if (even? len)
       (drop (reverse digits) (quotient len 2))
       (drop (reverse digits) (+ 1 (quotient len 2)))))

(define (next->prefix p)
  (let loop ([n p])
    (match n
      ['() '(1)]
      [(cons a next)
       (define new-a (add1 a))
       (if (= new-a 10)
           (cons 0 (loop next))
           (cons new-a next))])))

(define (digits->number digits)
  (for/fold
      ([acc 0])
      ([digit (reverse digits)])
    (+ (* 10 acc) digit)))

(define (prefix->invalid-number pref)
  (digits->number (append pref pref)))

(define (find-invalid-in-range lo hi)
  (generator
   ()
   (let loop ([pref (number->prefix lo)])
     (define inv-num (prefix->invalid-number pref))
     (when (<= inv-num hi)
       (when (>= inv-num lo)
         (yield inv-num))
       (loop (next->prefix pref))))))

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
