#lang racket
(define l (list 1 2 3 4))
(define (solve n)
  (define r (hash 0 1))
  (define v (list->vector (cons 1 (build-list n (lambda (x) 0)))))
  (define (solve-i i)
    (let* ([s (filter (lambda (x) (<= x i)) l)]
           [results-s (for/list ([x s]) (vector-ref v (- i x)))]
           [sum (foldl + 0 results-s)])
      (begin
        (vector-set! v i sum)
        sum)))
  (define (solve-all i)
    (begin
      (for/list ([x (range 1 (add1 i))])
        (solve-i x))
      (vector-ref v i)))
  (solve-all n))


