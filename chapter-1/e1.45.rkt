#lang sicp
(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (display guess)
    (newline)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define (average-damp f)
  (lambda (x) (average x (f x))))

(define (average a b)
  (/ (+ a b) 2))

(define (compose f g)
  (lambda (x) (f (g x))))

(define (repeated f n)
  (if (= n 1)
      f
      (compose f (repeated f (- n 1)))))

(define (exp y n)
  (if (= n 0)
      1
      (* y (exp y (- n 1)))))

(define (nth-rt x n damp-freq)
  (fixed-point
   ((repeated average-damp damp-freq)
    (lambda (y)
      (/ x (exp y (- n 1)))))
   1.0))

(define (sqrt x)
  (nth-rt x 2 1))

(define (cbrt x)
  (nth-rt x 3 1))

(define (4rt x)
  (nth-rt x 4 2))