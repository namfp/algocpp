#lang racket
(require racket/trace)
(require rackunit)

(define (get-index i j cmax)
    (+ (* i cmax) j))

(define (make-range from-i to-i)
  (range (min from-i to-i) (add1 (max from-i to-i)))
   )
  
(define (compute loop-i i j v cmax)
  (+ (cdr (vector-ref v (get-index loop-i (sub1 j) cmax) ))
     (foldr + 0 (map car 
                     (map (lambda (e) (vector-ref v (get-index e j cmax))) 
                              (make-range loop-i i))))))
(define (generate-vector l f cmax lmax)
  (define (nextij i j cmax lmax)
    (if (= j (sub1 cmax))
        (cons (add1 i) 0)
        (cons i (add1 j))))
  (define (g i j l f cmax lmax)
    (if (empty? l)
        null
        (let ([nij (nextij i j cmax lmax)])
          (cons (cons (car l) (f i j (car l)))
                (g (car nij) (cdr nij) (cdr l) f cmax lmax)))))  
  (list->vector (g 0 0 l f cmax lmax)))

(define (compute-min i j v cmax lmax)
  (apply min (map (lambda (loop-i) (compute loop-i i j v cmax)) (range lmax))))

(define (update-column j v cmax lmax)
  (for-each (lambda (e)
              (define index (get-index e j cmax))
              (vector-set! v index
                           (cons (car (vector-ref v index)) (compute-min e j v cmax lmax))))
            (range 0 lmax)))

(define (solve v cmax lmax)
  (for-each (lambda (i)
              (update-column i v cmax lmax))
            (range 1 lmax))
  (apply min (map (lambda (i) (cdr (vector-ref v (get-index i (sub1 cmax) cmax)))) (range 0 lmax)))
  )

(test-begin
 (check-equal? (get-index 0 0 10) 0)
 (check-equal? (get-index 2 7 10) 27)
 (check-equal? (make-range 5 10) (range 5 11))
 (check-equal? (make-range 7 3) (range 3 8))
 (define v (generate-vector '(2 4 5 4 3 4 5 8 7) (lambda (i j x) (sub1 x)) 3 3))
 (check-equal? (compute 1 0 2 v 3) 11)
 (check-equal? (compute-min 0 2 v 3 3) 8)
 (check-equal? (compute-min 2 2 v 3 3) 13) 
 )
