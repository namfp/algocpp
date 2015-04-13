#lang racket

(define (p76 n)
  (define v (vector-append #(0 1 2) (build-vector (- n 2) (lambda (x) 0))))
  (define (compute i)
    (begin
      (define r (for/sum  ([j (range 1 (sub1 i))])
                  (vector-ref v j)))
      (vector-set! v i r)
      r))
  (compute n))