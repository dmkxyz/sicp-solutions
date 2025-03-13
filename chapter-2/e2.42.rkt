#lang sicp
(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low (enumerate-interval (+ low 1) high))))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

(define (reverse items)
  (if (null? items)
      nil
      (append
       (reverse (cdr items))
       (list (car items)))))

(define (filter predicate sequence)
  (cond ((null? sequence) nil)
        ((predicate (car sequence))
         (cons (car sequence) (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))

(define empty-board nil)

(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? k positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position new-row k rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))

(define (adjoin-position new-row k rest-of-queens)
  (append rest-of-queens (list new-row)))

(define (safe? k positions)
  (let ((reversed (reverse positions))) 
   (and (check-same-rows reversed) (check-same-diagonals k reversed))))

(define (check-same-rows positions)
  (define (iter item positions)
    (cond
      ((null? positions) #t)
      ((= item (car positions)) #f)
      (else (iter item (cdr positions)))))
  (define first-item (car positions))
  (iter first-item (cdr positions)))

(define (check-same-diagonals k positions)
  (define first-item (car positions))
  (define rest-items (reverse (cdr positions)))
  (define (iter k index first-item rest-items)
    (cond
      ((null? rest-items) #t)
      ((= (- k index) (abs (- first-item (car rest-items)))) #f)
      (else (iter k (+ index 1) first-item (cdr rest-items)))))
  (iter k 1 first-item rest-items))


