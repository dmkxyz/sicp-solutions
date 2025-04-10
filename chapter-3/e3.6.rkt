#lang sicp
(define random-init 0)

(define rand
  (let ((x random-init))
    (define generate
      (begin (set! x (rand-update x))
             x))
    (define (reset val)
      (set! x val))
    (define (dispatch m)
      (cond ((eq? m 'reset) (lambda (x) (reset x)))
            ((eq? m 'generate) generate)))
    dispatch))
