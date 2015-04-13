#lang racket
(require racket/trace)
(require racket/list)
(define (generate max)
  (define (next_prime current f p)
    (if (> p max)
        current
        (let ([next_p (add1 p)]) 
          (if (> (remainder (remainder f p) 2) 0)
              (next_prime (cons p current) (* f (- next_p 2)) next_p)
              (next_prime current (* f (- next_p 2)) next_p)))))
  (next_prime null 1 2))

(define (sieve n)
  (define non-primes '())
  (define primes '())
  (for ([i (in-range 2 (add1 n))])
    (unless (member i non-primes)
      (set! primes (cons i primes))
      (for ([j (in-range (* i i) (add1 n) i)])
        (set! non-primes (cons j non-primes)))))
  (reverse primes))


(define (v-sieve max)
  (define matrix (make-vector (add1 max) true))
  (define (compute m i)
    (when (<= i max)
      (when (vector-ref m i)
        (for ([j (range 2 (add1 (quotient max i)))])
          (vector-set! matrix (* i j) false)))
      (compute m (add1 i))))
  (begin 
    (compute matrix 2)
    (define r null)
    (for/list ([i (range 2 max)])
      (when (vector-ref matrix i)
        (set! r (cons i r))))
    (list->vector (reverse r))
    ))

(define (v-generate-sum v-primes)
  (define r (vector-copy v-primes))
  (for ([i (range 0 (vector-length v-primes))])
    (unless (= i 0)  
      (vector-set! r i (+ (vector-ref r i) (vector-ref r (sub1 i))))))
  r
  )    



(define (generate-sum list_num)
  (cond [(empty? list_num) null]
        [(empty? (rest list_num)) (list (first list_num))]
        [else (let ([computed-rest (generate-sum (rest list_num))])
                (cons (+ (first list_num) (first computed-rest)) computed-rest))]))

(define (binary-search x v)
  ; loop : index index -> index or #f
  ;   return i s.t. l<=i<h and v[i]=x
  (define (loop l h)
    (cond [(>= l h) #f]
          [else (define m (quotient (+ l h) 2))
                (define y (vector-ref v m))
                (cond 
                  [(> y x) (loop l (- m 1))]
                  [(< y x) (loop (+ m 1) h)]
                  [else m])]))
  (loop 0 (vector-length v)))

(define (sumij i j v-primes v-sums)
  (if (= i 0)
      (vector-ref v-sums j)
      (- (vector-ref v-sums j) (vector-ref v-sums (sub1 i)))))

(define (find-max-nsums-i i j v-primes v-sums jmax sum-max max-prime max-n)
  (if (= j (vector-length v-sums)) 
      (list jmax sum-max max-n)
      (let ([current-sum (sumij i j v-primes v-sums)]
            [current-n (add1 (- j i))]
            )
        (if (> current-sum max-prime)
            (list jmax sum-max max-n)
            (if (and (> current-n max-n) (binary-search current-sum v-primes))
                (find-max-nsums-i i (add1 j) v-primes v-sums j current-sum max-prime current-n)
                (find-max-nsums-i i (add1 j) v-primes v-sums jmax sum-max max-prime max-n))))))

(define (find-max-nsums v-primes v-sums)
  (define max-prime (vector-ref v-primes (sub1 (vector-length v-primes))))
  (define computed-list (map (lambda (i) (cons i (find-max-nsums-i i 0 v-primes v-sums i 0 max-prime 0)))
                             (range 0 (vector-length v-primes))))
  (define (get-result computed)
    (argmax fourth computed-list))
  (get-result computed-list)
  )

(define solve
  (let* 
      ([define max-prime 1000000]
       [define v-primes (v-siege max-prime)]
       [define v-suns (v-generate-sum v-primes)])
    (find-max-nsums v-primes v-sums)
    ))
  
  ;
  ;(define (find-max-nsums i v-primes v-sums)
  ;  (define last-index (sub1 (length v-primes)))
  ;  (define f i j jmax v-primes v-sums
  ;  
  
  
  ;(define (largest-consecutive i-min v-primes v-sum)
  ;(trace find-max-nsums-i)
  