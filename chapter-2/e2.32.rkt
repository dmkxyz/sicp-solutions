#lang sicp
(define (subsets s)
  (if (null? s)
      (list nil)
      (let ((rest (subsets (cdr s))))
        (append rest (map (lambda (list-item) (cons (car s) list-item)) rest)))))

(subsets (list 1 2 3))