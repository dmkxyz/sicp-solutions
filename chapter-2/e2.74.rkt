#lang sicp
(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        (else ((get 'deriv (operator exp)) (operands exp) var))))

(define (operator exp) (car exp))

(define (operand exp) (cdr exp))

(define (variable? x) (symbol? x))

(define (same-variable? a1 a2)
  (and (variable? a1) (variable? a2) (eq? a1 a2)))
