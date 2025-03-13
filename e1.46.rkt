#lang sicp
(define tolerance 0.00001)

(define (iterative-improve good-enough? improve)
  (define (iter guess)
    (if (good-enough? guess)
        guess
        (iter (improve guess))))
  (lambda (guess) ( iter guess)))
                  
(define (average a b)
  (/ (+ a b)
     2))

(define (square x)
  (* x x))

(define (sqrt x)
  (define (close-enough? guess)
    (< (abs (- (square guess) x)) tolerance))
  (define (improve-sqrt-guess guess)
    (average guess (/ x guess)))
  ((iterative-improve close-enough? improve-sqrt-guess) 1.0))

(define (fixed-point f)
  (define (close-enough? guess)
    (< (abs (- guess (f guess))) tolerance))
  (define (improve guess)
    (f guess))
  ((iterative-improve close-enough? improve) 1.0))