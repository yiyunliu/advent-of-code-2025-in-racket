#lang racket

(define (main filename)
(with-input-from-file filename
  (lambda ()
    (for/fold
     ([acc 50]
      [num 0])
     ([l (in-lines)])
      (define result (regexp-match #rx"([LR])([0-9]+)" l))
      (define sign (if (string=? (second result) "L") - +))
      (define rotation-number (string->number (third result)))
      (define-values (quot rem) (quotient/remainder rotation-number 100))
      (define new-acc (sign acc rem))
      (define rem-zero (if (zero? acc)
                           0
                           (if (or (<= new-acc 0) (>= new-acc 100))
                               1
                               0)))
      (define new-num (+ (abs quot) rem-zero num))
      (values (modulo new-acc 100) new-num)))))
