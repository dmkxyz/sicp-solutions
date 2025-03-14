#lang sicp
(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause)))

(define (good-enough? guess x)
  (< (abs (- (/ guess (improve-guess guess x)) 1)) 0.001))

(define (improve-guess guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
          guess
          (sqrt-iter (improve-guess guess x)
                     x)))

(define (sqrt x)
  (sqrt-iter 1.0 x))

(sqrt 0.0000000000002)