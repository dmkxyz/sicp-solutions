#lang sicp
(define (fringe x)
  (cond ((null? x) x)
        ((not (pair? (car x))) (cons (car x) (fringe (cdr x))))
        (else (append (fringe (car x)) (fringe (cdr x))))))

(define x (list (list 1 2) (list 3 4)))

x
(fringe x)
(fringe (list x x))