#lang racket
(require racket/trace)

(define (solve n)
  (define l (range 3 (add1 n)))
  (define v (list->vector (append (list 1 1 1 2) (build-list (- n 3) (lambda (x) 0)))))
  (define (solve-i i)
    (let* ([s (filter (lambda (x) (>= (- i x 1) 0)) l)]
           [results-s (for/list ([x s]) (vector-ref v (- i x 1)))]
           [sum (+ (foldl + 0 results-s) (vector-ref v (- i 1)))])
      (begin
        (vector-set! v i sum)
        sum)))
  (trace solve-i)
  (define (solve-all i)
    (begin
      (for/list ([x (range 4 (add1 i))])
        (solve-i x))
      (vector-ref v i)))
  (solve-all n))
