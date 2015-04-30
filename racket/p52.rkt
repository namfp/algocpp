#lang racket

(define transform (compose1 list->set string->list number->string))
(define (f n mult)
  (cond [(empty? mult) true]
         [(equal? (transform n) (transform (* n (car mult)))) (f n (cdr mult))]
         [else false]))
         

(define (solve mult)
  (let loop ([k 1])
    (if (f k mult)
        k
        (loop (add1 k)))))
    
  