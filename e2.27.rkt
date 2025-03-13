#lang sicp
(define (deep-reverse x)
  (if (pair? x)
      (map deep-reverse (reverse x))
      x))

(define x (list (list 1 2) (list 3 4)))

x
(deep-reverse x)