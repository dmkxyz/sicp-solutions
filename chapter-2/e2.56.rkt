#lang sicp
(define (variable? x) (symbol? x))

(define (same-variable a1 a2)
  (and (variable? a1) (variable? a2) (eq? a1 a2)))

(define (sum? e)
  (eq? (car e) '+))

(define (addend e)
  (cadr e))

(define (augend e)
  (caddr e))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list '+ a1 a2))))

