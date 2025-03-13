#lang sicp
(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
     dx))

(define (cube x)
  (* x x x))

(define (simpson-integral f a b n)
  (define (h) (/ (- b a) (* n 1.0)))
  (define (y k) (f (+ a (* k (h)))))
  (define (inc x) (+ x 2))
  (* (/ (h) 3.0)
     (+ (y 0)
        (y n)
        (* 4.0
           (sum y 1 inc (- n 1)))
        (* 2.0
           (sum y 2 inc (- n 2)))
        )))

(define (sum-iter term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ result (term a)))))
  (iter a 0))