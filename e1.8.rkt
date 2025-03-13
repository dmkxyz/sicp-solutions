#lang sicp
(define (good-enough? guess x)
  (< (abs (- (* guess guess guess) x)) 0.001))

(define (improve-guess y x)
  (/ (+ (* 2 y)
        (/ x
           (* y y)))
     3))

(define (cbrt-iter guess x)
  (if (good-enough? guess x)
          guess
          (cbrt-iter (improve-guess guess x)
                     x)))

(define (cbrt x)
  (cbrt-iter 1.0 x))

(cbrt 12345)