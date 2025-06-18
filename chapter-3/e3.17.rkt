#lang sicp
(define (count-pairs x)
  (let ((seen '()))
    (define (iter-count x)
      (if (or (not (pair? x)) (memq x seen))
          0
          (begin
            (set! seen (cons x seen))
            (+ (iter-count (car x))
               (iter-count (cdr x))
               1))))
    (iter-count x)))

(define one-1 (cons 'a '()))
(define two-2 (cons 'a (cons 'b '())))
(define two-3 (cons one-1 one-1))
(define three-3 (cons 'a (cons 'b (cons 'c '()))))
(define three-4 (cons 'a (cons one-1 one-1)))
(define three-5 (cons two-2 two-2))
(define three-7 (cons two-3 two-3))

(count-pairs one-1)
(count-pairs two-2)
(count-pairs two-3)
(count-pairs three-3)
(count-pairs three-4)
(count-pairs three-5)
(count-pairs three-7)