#lang racket
(require data/heap)
(require racket/pretty)

(struct edge (from to distance))
(define edges1
  '([a . ((b . 1) (c . 2))]
    [b . ((f . 3) (d . 2))]
    [c . ((d . 3) (e . 4))]
    [d . ((f . 3) (g . 3) (e . 2))]
    [e . ((f . 5))]
    [f . ((g . 4))]
    ))

(define heap-distances (make-heap (lambda args (apply <= (map edge-distance args)))))

(define (partial-shortest l)
  (argmin (lambda (x) (edge-distance x)) l))

(define (next-nodes symbol hash-edges results)
  (filter (lambda (e) (not (member (edge-to e)
                                   (map edge-to results))))
          
          (hash-ref hash-edges symbol)))

(define (new-distance chosen-node nexts)
  (map (lambda (x) (edge (edge-from x) (edge-to x) (+ (edge-distance x) (edge-distance chosen-node))))
       nexts))


(define (new-step l results edges)
  (if (empty? l)
      results
      (let* ([shortest-node (partial-shortest l)]
             [nr (cons shortest-node results)]
             [removed-nodes (filter (lambda (x) (not (equal? (edge-to x) (edge-to shortest-node)))) l)]
             [nexts (next-nodes (edge-to shortest-node) edges nr)]
             [nd (new-distance shortest-node nexts)]
             [nl (append removed-nodes nd)]
             )
        (new-step nl nr edges)
        )))


(define (compute-edges raw-edges)
  (define r (make-hash))
  (begin
    (map (lambda (x) (let* ([node-from (car x)]
                            [dests (cdr x)])
                       (for ([dest dests])
                         (begin
                           (hash-set! r node-from (cons (edge node-from (car dest) (cdr dest)) 
                                                        (hash-ref! r node-from null)))
                           (hash-set! r (car dest) (cons (edge (car dest) node-from (cdr dest)) 
                                                         (hash-ref! r (car dest) null)))
                           
                           ))))
         raw-edges
         )
    r
    ))

(define (dijkstra node-from graph)
  (define origin-node (edge 'a 'a 0))
  (new-step (new-distance origin-node (next-nodes 'a graph (list origin-node)))
            (list origin-node)
            graph))
