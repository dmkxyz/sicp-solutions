#lang sicp

(define (f n)
  (if (< n 3)
      n
      (f-iter 2 1 0 0 n)))

(define (f-iter a b c count n)
  (cond ((> count (- n 3)) a)
        (else (f-iter (+ a (* 2 b) (* 3 c))
                      a
                      b
                      (+ count 1)
                      n
               ))))

(#%require racket/trace)
(trace f-iter)