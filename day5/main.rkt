#lang racket

(require data/interval-map)
(require racket/generator)

(define (parse-intervals p)
  (define r (make-interval-map))
  (for ([line (sequence-map string-trim (in-lines p))])
    #:break (string=? line "")
    (define result (regexp-match "([0-9]+)-([0-9]+)" line))
    (define start (string->number (second result)))
    (define end (add1 (string->number (third result))))
    (interval-map-set! r start end #t))
  r)

(define (parse-queries p)
  (generator
   ()
   (for ([line (sequence-map string-trim (in-lines p))])
    #:break (string=? line "")
    (yield (string->number line)))))

(define (main p)
  (define r (parse-intervals p))
  (define queries (parse-queries p))
  (for/sum ([query (in-producer queries (void))])
    (if (interval-map-ref r query #f) 1 0)))

(define (main-p2 p)
  (define r (parse-intervals p))
  (define gen
    (generator
     ()
     (let loop ([iter (interval-map-iterate-first r)])
       (when iter
         (yield (interval-map-iterate-key r iter))
         (loop (interval-map-iterate-next r iter))))))
  (for/sum
      ([interval (in-producer gen (void))])
    (- (cdr interval) (car interval))))

(provide main main-p2)
