#lang racket
(require racket/trace)
(require rackunit)
(require math/matrix)
(provide add-new-distance)
(provide partial-shortest)
(provide new-distance)
(provide decompose)
(provide next-node)
(provide add-one-distance)
(provide compute-hash-edges)
(provide vector->edges)

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

(define (compute-hash-edges raw-edges)
  (compute-hash-edges-loop (make-hash) raw-edges))

(define (compute-hash-edges-loop results raw-edges)
  (define (compute-hash-one results from to distance)
    (define current-list
      (if (hash-has-key? results from)
          (hash-ref results from)
          null))
    (hash-set! results from (cons (cons to distance) current-list))
    )
  
  (define (compute-hash-edge results edge)
    (begin
      (for-each (lambda (e) (compute-hash-one results (car e) (car edge) (cdr e))) (cdr edge))
      (for-each (lambda (e) (compute-hash-one results (car edge) (car e) (cdr e))) (cdr edge))
      (void)))
  
  (begin
    (for-each (lambda (e) (compute-hash-edge results e)) raw-edges)
    results))


(define (partial-shortest l)
  (define elems (apply append (map decompose l)))
  (argmin (lambda (x) (car (cdr x))) elems))

(define (decompose elem)
  (map (lambda (x) (cons (car elem) x)) (cdr elem)))

(define (next-node symbol hash-edges results)
  (filter (lambda (e) (not (member (car e)
                                   (map car results))))
          
          (hash-ref hash-edges symbol)))

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
             [nexts (next-node s edges nr)]
             [nd (new-distance m nexts)]
             [nl (add-new-distance removed-nodes nd)]
             )
        (new-step nl nr edges)
        )))

;(new-step '((b . ((1 . a))) (c . ((2 . a)))) '((a . (0 . a))) (compute-hash-edges edges1))

(define (dijkstra node-from graph)
  (define origin-node (cons node-from (cons 0  node-from)))
  (new-step (add-new-distance null 
                              (new-distance origin-node
                                            (next-node node-from graph null)))
            (list origin-node)
            graph))

(define (vector->edges v-edges lmax cmax)
  (define results (make-hash))
  (define imax (sub1 lmax))
  (define jmax (sub1 cmax))
  (define (get-index i j)
    (+ (* i cmax) j))
  (define (get-val i j)
    (vector-ref v-edges (get-index i j)))
  (define (update-results i j func-i func-j)
    (hash-set! results (get-index i j) 
               (cons (cons (get-index (func-i i) (func-j j))
                           (get-val (func-i i) (func-j j)))
                     (hash-ref! results (get-index i j) null))))
  
  (for ([i (range lmax)])
    (for ([j (range cmax)])
      (when (< j jmax)
        (update-results i j (lambda (x) x) add1))
      (when (< i imax)
        (update-results i j add1 (lambda (x) x)))
      (when (> i 0)
        (update-results i j sub1 (lambda (x) x)))
      (when (> j 0)
        (update-results i j (lambda (x) x) sub1))))
  results
  )

(define (solve v lmax cmax)
  (define (get-index i j)
    (+ (* i cmax) j))
  (define (solve-j j)
    (solve-j (sub1 j))
    (for ([i (range 0 imax)])
      (define elem (vector-get v (get-index i j)))
      (define mval (argmin (map (lambda (loop-i) (compute loop-i i j) 
                                (range 0 lmax)))
      (vector-set! v (get-index i j) (cons  (car elem) mval))
      
    




(trace new-step)
(trace partial-shortest)
(trace new-distance)
(trace partial-shortest)
;
;
;(check-equal? (partial-shortest '((b . (2 . a)) (d . (3 . b)))) '(b . (2 . a)))

