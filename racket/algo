#lang racket

(define (generate max)
  (define (next_prime current f p)
    (if (> p max)
        current
        (let ([next_p (add1 p)]) 
          (if (> (remainder (remainder f p) 2) 0)
              (next_prime (cons p current) (* f (- next_p 2)) next_p)
              (next_prime current (* f (- next_p 2)) next_p)))))
    (next_prime null 1 2))
  
(define (generate_sum list_num)
  (cond [(empty? list_num) null]
        [(empty? (rest list_num)) (first list_num)]
        [else (let ([computed_rest (generate_sum (rest list_num))])
                (cons (+ (first list_num) (first computed_rest) computed_rest)))]))