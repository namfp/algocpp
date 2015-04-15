#lang racket
(require rackunit)
(define (add x y)
  (+ x y))
(define edges
  '([a . ((b 7)(c 9)(f 14))]
    [b . ((c 10)(d 15))]
    [c . ((d 11)(f 2))]
    [d . ((e 6))]
    [e . ((f 9))]))

(define (partial-shortest l)
  (argmin (lambda (x) (cadr x)) l))

(define (next-node symbol edges)
  (cdr (findf (lambda (x) (equal? (car x) symbol)) edges)))


(define (new-step l)
  (define m (partial-shortest l))
  1)
  

(define test (make-hash (list (cons 1 2))))

(check-eq? (add 1 2) 3)

(check-equal? (partial-shortest '((b . ((1 . a))) (c . ((2 . a))) (d . ((+inf.0 . a))))) '(b . (1 . a))) 
(check-equal? (partial-shortest '((b . (2 . a)) (d . (3 . b)))) '(b . (2 . a)))
(check-equal? (next-node 'b edges) '((c 10)(d 15)))