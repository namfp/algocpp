#lang racket
(require racket/trace)
(define (g k)
  (cons (/ (* k (- (* 3 k) 1)) 2) (expt -1 (sub1 k)))
  )

(define (generate-p n)
  
  (define  (loop k r)
    (let* [(gk (g k))
           (gkn (g (* -1 k)))
           (l (filter (lambda (x) (<= (car x) n)) (list gk gkn)))
           ]
      
      (if (empty? l)
          r
          (append l (loop (add1 k) r)))))
  (loop 1 null)
  )

(define (partition n memo)
  ;(define memo (make-hash (list (cons 0 1))))
  (define pn (generate-p n))
  (define (loop k)
    (cond [(hash-has-key? memo k) (hash-ref memo k)]
          [(negative? k) 0]
          [(= k 0) 1]
          [else
           (let* ([t (takef pn (lambda (x) (<= (car x) k)))]
                  [r (foldl + 0 (map (lambda (i) (* (cdr i) (loop (- k (car i)))))
                                     t))])
             (begin (hash-set! memo k r)
                    r
                    ))]))
  (loop n))

(define memo (make-hash))

(define solve
  (let loop ([k 1])
             (let ([pk (partition k memo)])
               (if (= (remainder pk 1000000) 0)
                   k
                   (loop (add1 k))))))
