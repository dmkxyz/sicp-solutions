#lang sicp

(define (even? x)
  (= (remainder x 2) 0))

(define (double x)
  (+ x x))

(define (halve x)
  (/ x 2))

(define (* a b)
  (f a b 0))

(define (f a b m)
  (cond ((= b 0) m)
        ((even? b)
         (f (double a) (halve b) m))
        (else (f a (- b 1) (+ m a)))))

(#%require racket/trace)
(trace f)