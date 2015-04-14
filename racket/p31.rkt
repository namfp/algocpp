#lang racket
(define (solve sum)
  (define l '(1 2 5 10 20 50 100 200))
  (define memo (make-hash))
  (define (compute i)
    (for/sum (map (lambda (j) (compute (- n j)))
                  (filter (lambda (j) (positive? (- n j))) l)))
    
    