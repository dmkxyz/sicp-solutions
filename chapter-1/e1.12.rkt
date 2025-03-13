#lang sicp
(define (pascal row col)
  (cond ((= col 0) 1)
        ((= col row) 1)
        (else (+ (pascal (- row 1) (- col 1))
                 (pascal (- row 1) col)))))

(define (display-pascal-row r)
  (define (display-col-iter c)
    (if (= r c)
        (newline)
        (begin
         (display (pascal (- r 1) c))
         (display " ")
         (display-col-iter (+ c 1))
         )))
  (display-col-iter 0))
  
(define (display-pascal n)
  (define (display-pascal-iter i)
    (display-pascal-row i)
    (if (= i n)
        (newline)
        (display-pascal-iter (+ i 1))))
  (display-pascal-iter 1))

  