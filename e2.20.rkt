#lang sicp


(define (same-parity . l)
  (define isodd (remainder (car l) 2))
  (define (iter l)
      (cond ((null? l) nil)
            ((= isodd (remainder (car l) 2)) (cons (car l) (iter (cdr l))))
            (else (iter (cdr l)))))
    (iter l))