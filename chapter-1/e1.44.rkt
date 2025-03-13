#lang sicp
(define (inc x)
  (+ x 1))

(define (square x)
  (* x x))

(define (compose f g)
  (lambda (x) (f (g x))))

(define (repeated-a f n)
  (lambda (x)
    (if (= n 1)
         f
         ((repeated ((compose f f) x) (- n 1)) x))))

(define (repeated f n)
  (if (= n 1)
      f
      (compose f (repeated f (- n 1)))))

(define dx 0.00001)

(define (smooth f)
  (lambda (x) (/
               (+
                (f (- x dx))
                (f x)
                (f (+ x dx)))
               3)))

(define (n-smooth f n)
  ((repeated smooth n) f))