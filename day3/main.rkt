#lang racket
(require data/gvector)
(require racket/generator)


(define-struct Digit (digit) #:transparent)

(define (tokenize p)
  (generator
   ()
   (let loop ()
     (define ch (read-char p))
     (when (not (eof-object? ch))
       (cond
         [(char-numeric? ch)
          (yield (Digit (- (char->integer ch) (char->integer #\0))))
          (loop)]
         [(char-blank? ch)
          (loop)]
         [(char=? ch #\newline)
          (yield 'newline)
          (loop)])))))

(define (add-up total left right)
  (if (and left right)
      (+ total (* left 10) right)
      total))

(define (combine-lr left right)
  (+ (* 10 left) right))

(define (main port)
  (for/fold
      ([total 0]
       [line-max 0]
       [left #f]
       [right #f])
      ([token (in-producer (tokenize port) (void))])
    (match token
      [(Digit digit)
       (cond
         [(false? left)
          (values total (max digit line-max) digit #f)]
         [(false? right)
          (values total (max (combine-lr left digit) line-max) left digit)]
         [else
          (cond
            [(and (> digit left) (> digit right))
             (values total
                     (max (combine-lr (max left right) digit) line-max)
                     digit
                     #f)]
            [(> right left)
             (values total
                     (max (combine-lr right digit) line-max)
                     right
                     digit)]
            [(> digit right)
             (values total
                     (max line-max (combine-lr left digit))
                     left
                     digit)]
            [else
             (values total
                     line-max
                     left
                     right)])])]
      ['newline
       (values (+ total line-max) 0 #f #f)])))
