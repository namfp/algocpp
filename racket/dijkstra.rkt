#lang racket
(require data/heap)
(require racket/pretty)

(struct edge (from to distance))
(define (display-edge e)
  (printf "from: ~a to: ~a distance: ~a" (edge-from e) (edge-to e) (edge-distance e)))

(define edges1
  '([a . ((b . 1) (c . 2))]
    [b . ((f . 3) (d . 2))]
    [c . ((d . 3) (e . 4))]
    [d . ((f . 3) (g . 3) (e . 2))]
    [e . ((f . 5))]
    [f . ((g . 4))]
    ))

(define m (list->vector (map list->vector
           '((131 673 234 103 18)
            (201 96 342 965 150)
            (630 803 746 422 111)
            (537 699 497 121 956)
            (805 732 524 37 331)))))

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

(define (matrix->raw matrix len)
  (define r (make-hash))
  (define (add-node-one-way node-from node-to distance)
    (hash-set! r node-from (cons (edge node-from node-to distance)
                                 (hash-ref! r node-from null))))
  (define (add-node-two-ways node-from node-to distance)
    (begin (add-node-one-way node-from node-to distance)
           (add-node-one-way node-to node-from distance)))
  
  (for* ([i (range len)]
         [j (range len)])
    (let ([index (+ (* i len) j)]
          [get-val (lambda (i j) (vector-ref (vector-ref matrix i) j))])
      (begin
        (when (< j (sub1 len))
          (add-node-two-ways index (add1 index) (+ (get-val i j) (get-val i (add1 j)))))
        (when (< i (sub1 len))
          (add-node-two-ways index (+ index len) (+ (get-val i j) (get-val (add1 i) j)))))))
  r
  
  )

(define (solve filename)
  (define v (list->vector (map (lambda (l) 
                                 (list->vector (map string->number (string-split l ",")))) 
                               (file->lines "p083_matrix.txt"))))
  (define raw (dijkstra 0 (matrix->raw v 80)))
  (define final-node (findf (lambda (e) (= (edge-to e) (sub1 (* 80 80)))) (dijkstra 0 (matrix->raw m 5))))
  (define result (/ (+ (edge-distance final-node) 4445 7981) 2))
  result)





(define (dijkstra node-from graph)
  (define s node-from)
  (define origin-node (edge s s 0))
  (new-step (new-distance origin-node (next-nodes s graph (list origin-node)))
            (list origin-node)
            graph))
