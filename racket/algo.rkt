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
        [(empty? (rest list_num)) (list (first list_num))]
        [else (let ([computed_rest (generate_sum (rest list_num))])
                (cons (+ (first list_num) (first computed_rest)) computed_rest))]))

(define (binary_search elem vector_nums)
  (define (binary_search_slice elem vector_nums left right)
    (let ([mid (quotient (+ left right) 2)]) 
      (cond
        [(= elem (vector-ref vector_nums right)) true]
        [(= elem (vector-ref vector_nums left)) true]
        [(> elem (vector-ref vector_nums right)) false]
        [(< elem (vector-ref vector_nums left)) false]
        [(= left right) (= elem (vector-ref vector_nums left))]
        [(= elem (vector-ref vector_nums mid)) true]
        [(< elem (vector-ref vector_nums mid)) (binary_search_slice elem vector_nums left mid)]
        [else (binary_search_slice elem vector_nums mid right)])))
  (binary_search_slice elem vector_nums 0 (sub1 (vector-length vector_nums))))
