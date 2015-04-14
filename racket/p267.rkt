#lang racket

(define f 0.25)
(define n 1000)
  
(define r (foldl + 0 (for/list ([i (range 0 1001)])
               (* (expt (+ 1 (* 2 f)) n) (expt (- 1 f) (- n i))))))