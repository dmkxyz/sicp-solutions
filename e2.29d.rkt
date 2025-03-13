#lang sicp
(define (make-mobile left right)
  (cons left right))

(define (make-branch length structure)
  (cons length structure))

(define left-branch car)

(define right-branch cdr)

(define branch-length car)

(define branch-structure cdr)

(define (total-weight mobile)
  (cond
    ((number? mobile) 0)
    ((number? (right-branch mobile)) (right-branch mobile))
    (else
     (+ (total-weight (left-branch mobile))
            (total-weight (right-branch mobile))))))

(define (is-balanced mobile)
  (cond
    ((number? (right-branch mobile)) #t)
    ((number? (left-branch mobile)) (is-balanced (right-branch mobile)))
    ((and
       (is-balanced (left-branch mobile))
       (is-balanced (right-branch mobile))
       (=
         (* (branch-length (left-branch mobile)) (total-weight (left-branch mobile)))
         (* (branch-length (right-branch mobile)) (total-weight (right-branch mobile))))) #t)
    (else #f)))
      
(define mobile (cons (cons 3 (cons (cons 4 5) (cons 2 10))) (cons 9 5)))


(is-balanced mobile)