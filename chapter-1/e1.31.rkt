#lang sicp
(define (product term a next b)
  (if (> a b)
      1
      (* (term a)
         (product term (next a) next b))))

(define (product-iter term a next b result)
  (if (> a b)
      result
      (product-iter term (next a) next b (* result (term a)))))
      
(define (factorial n)
  (define (identity x) x)
  (define (inc x) (+ x 1))
  (product-iter identity 1 inc n 1))

(define (pi n)
  (define (inc x) (+ x 2))
  (* 4.0
     (/ (* 2.0
           (product-iter square 4.0 inc n 1)
           (+ n 2))
        (product-iter square 3.0 inc (+ n 1) 1))))

(define (square x) (* x x))

(define (wallis-product n)
  (define (term n)
    (* (/ (* 2 n)
          (- (* 2 n) 1))
       (/ (* 2 n)
          (+ (* 2 n) 1))))
  (product term 1.0 inc n))