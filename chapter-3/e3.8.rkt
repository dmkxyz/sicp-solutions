#lang sicp
(define (f x)
  (let ((called #f))
    (cond  ((and (eq? called #f) (= x 1))
            (begin (set! called #t)
                  x))
          (else (begin (set! called #t)
                  0)))))