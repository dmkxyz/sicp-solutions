#lang sicp
(define (length items)
  (if (null? items)
      0
      (+ 1 (length (cdr items)))))

(define (list-ref items n)
  (if (= n 0)
      (car items)
      (list-ref (cdr items) (- n 1))))

(define (last-pair items)
  (if (null? (cdr items))
      items
      (last-pair (cdr items))))

