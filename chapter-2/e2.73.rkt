#lang racket

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))
(define *op-table* (make-hash))

(define (put op type proc)
  (hash-set! *op-table* (list op type) proc))

(define (get op type)
  (hash-ref *op-table* (list op type) '()))

  (define (deriv exp var)
    (cond ((number? exp) 0)
          ((variable? exp) (if (same-variable? exp var) 1 0))
          (else ((get 'deriv (operator exp)) (operands exp) var))))

(define (operator exp) (car exp))

  (define (operands exp) (cdr exp))

  (define (variable? x) (symbol? x))

  (define (same-variable? a1 a2)
    (and (variable? a1) (variable? a2) (eq? a1 a2)))

(define (install-deriv-package)
  ;; internal procedures
  (define (=number? exp num)
    (and (number? exp) (= exp num)))

  (define (addend e) (car e))

  (define (augend e) (accumulate make-sum 0 (cdr e)))

  (define (make-sum a1 a2)
    (cond ((=number? a1 0) a2)
          ((=number? a2 0) a1)
          ((and (number? a1) (number? a2)) (+ a1 a2))
          (else (list '+ a1 a2))))

  (define (multiplier e) (car e))

  (define (multiplicand e) (accumulate make-product 1 (cdr e)))

  (define (make-product m1 m2)
    (cond ((or (=number? m1 0) (=number? m2 0)) 0)
          ((=number? m1 1) m2)
          ((=number? m2 1) m1)
          ((and (number? m1) (number? m2)) (* m1 m2))
          (else (list '* m1 m2))))

  (define (make-exponentiation b e)
    (cond ((=number? e 0) 1)
          ((=number? e 1) b)
          ((=number? b 1) 1)
          (else (list '** b e))))

  (define (exponentiation? exp)
    (and (pair? exp) (eq? (car exp) '**)))

  (define (base exp)
    (car exp))

  (define (exponent exp)
    (cadr exp))
  
  ;; interface to the rest of the system
  (put 'deriv '+ (lambda (operands var) (make-sum (deriv (addend operands) var)
                                                 (deriv (augend operands) var))))

  (put 'deriv '* (lambda (operands var) (make-sum
                                        (make-product (multiplier operands)
                                                      (deriv (multiplicand operands) var))
                                        (make-product (deriv (multiplier operands) var)
                                                      (multiplicand operands)))))

  (put 'deriv '** (lambda (operands var) (make-product (exponent operands) 
                                                       (make-product
                                                        (make-exponentiation (base operands) (- (exponent operands) 1))
                                                        (deriv (base operands) var)))))

  'done)

(install-deriv-package)