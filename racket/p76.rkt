#lang racket


(require racket/trace)
(define (solve sum)
  (define l (reverse (rest (range sum))))
  (define (generate-sum sum coin-value)
    (if (< (- sum coin-value) 0) 
        (list sum)
        (cons sum (generate-sum (- sum coin-value) coin-value))))
  (define (compute n coins)
    (define filtered-coins (filter (lambda (x) (<= x n)) coins))
    (cond 
      [(= n 0) 1]
      [(null? filtered-coins) 0]
          [else (apply + (map (lambda (s) (compute s (rest filtered-coins)))
                              (generate-sum n (first filtered-coins))))]))
  
  (compute sum l))


