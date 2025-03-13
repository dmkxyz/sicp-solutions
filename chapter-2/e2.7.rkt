#lang sicp
(define (make-interval a b)
  (cons a b))

(define (upper-bound x)
  (cdr x))

(define (lower-bound x)
  (car x))

(define (add-interval x y)
  (make-interval (+ (lower-bound x)
                    (lower-bound y))
                 (+ (upper-bound x)
                    (upper-bound y))))

(define (sub-interval x y)
  (make-interval (- (lower-bound x)
                    (lower-bound y))
                 (- (upper-bound x)
                    (upper-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (div-interval x y)
  (if (or (= 0 (lower-bound y)) (= 0 (upper-bound y)))
      (begin
        (newline)
        (display "error:")
        (display y)
        (display " interval spans 0")
        (newline))
      (mul-interval x
                    (make-interval (/ 1.0 (upper-bound y))
                                   (/ 1.0 (lower-bound y))))))

(define (width x)
  (/ (- (upper-bound x)
        (lower-bound x))
     2))


(define i1 (make-interval 3 6))
(define i2 (make-interval 7 0))

(define i1xi2 (mul-interval i1 i2))
(define i1di2 (div-interval i1 i2))

(= (width i1xi2)
   (* (width i1)
      (width i2)))


