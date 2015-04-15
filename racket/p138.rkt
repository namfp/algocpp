#lang racket
(require racket/trace)
(define (solve max)
  (define (computexy n)
    (if (equal? n 0)
        (list 0 -1 null)
        (let* ([r (computexy (sub1 n))]
              [preced-x (first r)]
              [preced-y (second r)]
              [preced-results (third r)]
              [xn (+ (* -9 preced-x) (* -4 preced-y) -4)]
              [yn (+ (* -20 preced-x) (* -9 preced-y) -8)])
          (list xn
                yn
                (if (equal? xn 0) 
                    preced-results
                    (cons (abs yn) preced-results))))))
  (trace computexy)
  
    (apply + (third (computexy (add1 max)))))