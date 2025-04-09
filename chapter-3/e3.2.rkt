#lang sicp

(define (make-monitored f)
  (let ((calls 0))
   (lambda (x)
     (cond ((eq? x 'how-many-calls?) calls)
           ((eq? x 'reset-count) (set! calls 0))
           (else (begin (set! calls (+ calls 1))
                        (f x)))))))