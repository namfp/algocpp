#lang racket
(require rackunit)
(require "p82.rkt")
(define edges
  '([a . ((b 7)(c 9)(f 14))]
    [b . ((c 10)(d 15))]
    [c . ((d 11)(f 2))]
    [d . ((e 6))]
    [e . ((f 9))]))

(check-equal? (add-one-distance '((b . ((5 . c) (1 . a))) 
                                  (c . ((2 . a))) 
                                  (d . ((1 . a)))
                                  )
                                '(c . (2 . b)))
              '((c . ((2 . b) (2 . a))) (b . ((5 . c) (1 . a))) (d . ((1 . a)))))

(check-equal? (decompose  '(b . ((1 . a) (2 . c)))) '((b . (1 . a)) (b . (2 . c))))

(check-equal? (next-node 'b edges) '((c 10)(d 15)))

(check-equal? (new-distance '(b . (2 . a)) '((c . 1) (d . 2))) '((c . (3 . b))  (d . (4 . b))))

(check-equal? (add-one-distance '((b . ((5 . c) (1 . a))) 
                                  (c . ((2 . a))) 
                                  (d . ((1 . a)))
                                  (e . ((7 . a)))
                                  )
                                '(c . (2 . b)))
              '((c . ((2 . b) (2 . a))) (b . ((5 . c) (1 . a))) (d . ((1 . a))) (e . ((7 . a)))))



(check-equal? (add-new-distance '((b . ((5 . c) (1 . a))) 
                                  (c . ((2 . a))) 
                                  (d . ((1 . a))))
                                '((c . (2 . b)) (d . (2 . a))))
              '((d . ((2 . a) (1 . a))) (c . ((2 . b) (2 . a))) (b . ((5 . c) (1 . a)))))



(check-equal? (partial-shortest '((b . ((5 . c) (1 . a))) 
                                  (c . ((2 . a))) 
                                  (d . ((+inf.0 . a))))) 
              '(b . (1 . a))) 

(check-equal? (next-node
   'f
   '((a (b . 7) (c . 9) (f . 14))
     (b (c . 10) (d . 15))
     (c (d . 11) (f . 2))
     (d (e . 6))
     (e (f . 9)))) null)