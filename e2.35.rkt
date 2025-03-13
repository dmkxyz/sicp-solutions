#lang sicp
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (count-leaves t)
  (accumulate
   +
   0
   (map (lambda (item) (if (not (pair? item)) 1 (count-leaves item))) t)))

(define x (cons (list 1 2) (list 3 4)))
(define y (cons 2 (list (list 4 5))))

(count-leaves x)
(count-leaves y)

(count-leaves '(1 2 (3 (4) 5) (6 7)))