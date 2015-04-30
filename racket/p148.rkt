#lang racket

(define (generate-pascal-line line)
  (cons (car line)
        (let loop ([l line])
          (match l
            [(list a b _ ...) (cons (+ a b) (loop (rest l)))]
            [(list a) (list a)]
            [_ null]
            )))
  )
   
(struct result (line count))
  

(define (f n)
  (if (= n 1)
      (result (list 1) 1)
      (let* ([r (f (sub1 n))]
             [new-line (generate-pascal-line (result-line r))]
             [new-count (+ (count (lambda (x) (not (= (remainder x 7) 0))) new-line) (result-count r))]
             )
        (result new-line new-count))))
    

;(define solve
;  (let*-values ([(last-line last-count) (f (expt 10 9))])
;    last-count))
;    
  