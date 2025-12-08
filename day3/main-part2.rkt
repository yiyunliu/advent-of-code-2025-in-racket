#lang racket
(require data/gvector)
(require racket/generator)


(define-struct Digit (digit) #:transparent)

(define (tokenize p)
  (generator
   ()
   (let loop ()
     (define ch (read-char p))
     (if (eof-object? ch)
         (begin (yield 'newline) (void))
         (cond
           [(char-numeric? ch)
            (yield (Digit (- (char->integer ch) (char->integer #\0))))
            (loop)]
           [(char-blank? ch)
            (loop)]
           [(char=? ch #\newline)
            (yield 'newline)
            (unless (eof-object?(peek-char p))
                (loop))])))))


(define (parse-line p)
  (generator
   ()
   (define gvec (gvector))
   (for ([token (in-producer p (void))])
     (match token
       ['newline
        (yield (gvector->vector gvec))
        (set! gvec (gvector))]
       [(Digit d) (gvector-add! gvec d)]))))

(define (draft-n n nums)
  (define len (vector-length nums))
  (define stack (gvector))
  (define (size) (gvector-count stack))
  (define (top) (gvector-ref stack (sub1 (size))))
  (define (pop!) (gvector-remove-last! stack))
  (define (push! v) (gvector-add! stack v))
  (for ([(num idx) (in-indexed nums)])
    (let loop ()
      (cond
        [(zero? (size))
         (push! num)]
        [(and (> num (top))
              (> (+ (size) (- len idx)) n))
         (pop!)
         (loop)]
        [(< (size) n)
         (push! num)]
        [else
         (void)])))
  (for/fold
      ([acc 0])
      ([v stack])
    (+ (* acc 10) v)))


(define (main n port)
  (define gen (parse-line (tokenize port)))
  (for/sum
      ([v (sequence-map (lambda (v) (draft-n n v)) (in-producer gen (void)))])
    v))
