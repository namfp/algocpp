#lang racket
(require racket/trace)
(require rackunit)
(provide add-new-distance)
(provide partial-shortest)
(provide new-distance)
(provide decompose)
(provide next-node)
(provide add-one-distance)

(define edges
  '([a . ((b . 7) (c . 9) (f . 14))]
    [b . ((c . 10) (d . 15))]
    [c . ((d . 11) (f . 2))]
    [d . ((e . 6))]
    [e . ((f . 9))]
    ))

(define edges1
  '([a . ((b . 1) (c . 2))]
    [b . ((f . 3) (d . 2))]
    [c . ((d . 3) (e . 4))]
    [d . ((f . 3) (g . 3) (e . 2))]
    [e . ((f . 5))]
    [f . ((g . 4))]
    ))

(define (partial-shortest l)
  (define elems (apply append (map decompose l)))
  (argmin (lambda (x) (car (cdr x))) elems))

(define (decompose elem)
  (map (lambda (x) (cons (car elem) x)) (cdr elem)))

(define (next-node symbol edges)
  (let ([found (findf (lambda (x) (equal? (car x) symbol)) edges)])
    (if found
        (cdr found)
        null
        )))

(define (new-distance chosen-node nexts)
  (map (lambda (x) (cons (car x) 
                         (cons (+ (cadr chosen-node) (cdr x)) 
                               (car chosen-node))))
       nexts))

(define (add-one-distance l distance)
  (match distance [(cons node (cons d node-from))
                   (let-values ([(found rest) (partition (lambda (x) (equal? (car x) node)) l)])
                     (if (empty? found)
                         (cons (cons node (list (cons d node-from))) rest)
                         (cons (cons (caar found) (cons (cons d  node-from)  (cdar found))) 
                               rest)))]))

(define (add-new-distance l new-distances)
  (if (empty? new-distances)
      l
      (add-new-distance (add-one-distance l (first new-distances)) (rest new-distances))))


(define (new-step l results edges)
  (if (empty? l)
      results
      (let* ([m (partial-shortest l)]
             [nr (cons m results)]
             [s (car m)]
             [removed-nodes (filter (lambda (x) (not (equal? (car x) s))) l)]
             [nexts (next-node s edges)]
             [nd (new-distance m nexts)]
             [nl (add-new-distance removed-nodes nd)]
             )
        (new-step nl nr edges)
        )))
(trace new-step)
;
;
;(check-equal? (partial-shortest '((b . (2 . a)) (d . (3 . b)))) '(b . (2 . a)))
