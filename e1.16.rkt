#lang sicp
(define (f b n)
  (if (= n 0)
      1
      (fi b n 1)))

(define (fi b n a)
  (cond ((= n 0) a)
        ((even? n) (fi (square b) (/ n 2) a))
        (else (fi b (- n 1) (* a b)))))

(define (square b)
  (* b b))

(#%require racket/trace)
(trace fi)
(f 2 10)