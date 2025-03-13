#lang sicp

(define (for-each proc list)
  (if (null? list)
      #f
      (begin
        (proc (car list))
        (for-each proc (cdr list)))))


(for-each (lambda (x) (newline) (display x))
          (list 57))