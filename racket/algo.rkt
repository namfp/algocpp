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

(define (sumij i j v-primes v-sums)
  (if (= i 0)
      (vector-ref v-sums j)
      (- (vector-ref v-sums j) (vector-ref v-primes (sub1 i)))))

(define (find-max-nsums-i i j v-primes v-sums jmax sum-max max-prime max-n)
  (if (= j (vector-length v-sums)) 
      (list jmax max-n)
      (let ([current-sum (sumij i j v-primes v-sums)]
            [current-n (add1 (- j i))]
            )
        (if (> current-sum max-prime)
            (list jmax max-n)
            (if (and (> current-n max-n) (vector-member current-sum v-primes))
                (find-max-nsums-i i (add1 j) v-primes v-sums j current-sum max-prime current-n)
                (find-max-nsums-i i (add1 j) v-primes v-sums jmax sum-max max-prime max-n))))))

(define (find-max-nsums v-primes v-sums)
  (define max-prime (vector-ref v-primes (sub1 (vector-length v-primes))))
  (define computed-list (map (lambda (i) (cdr (find-max-nsums-i i 0 v-primes v-sums 0 0 max-prime 0)))
                    (range 0 (vector-length v-primes))))
  computed-list
  )
 
    
                    

;
;(define (find-max-nsums i v-primes v-sums)
;  (define last-index (sub1 (length v-primes)))
;  (define f i j jmax v-primes v-sums
;  


;(define (largest-consecutive i-min v-primes v-sum)

(define v-primes (list->vector (reverse (generate 100))))
(define v-sums (list->vector (reverse (generate_sum (generate 100)))))
