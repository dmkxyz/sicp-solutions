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

(define (filter predicate sequence)
  (cond ((null? sequence) nil)
        ((predicate (car sequence))
         (cons (car sequence) (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))

(define (unique-pairs n)
  (flatmap 
   (lambda (i)
         (map (lambda (j) (list i j))
              (enumerate-interval 1 (- i 1))))
       (enumerate-interval 1 n)))

(define (unique-triples n)
  (flatmap
   (lambda (i)
     (map (lambda (pair)
          (cons i pair))
          (unique-pairs (- i 1))))
   (enumerate-interval 1 n)))

(define (check-sum triple s)
  (= s
     (accumulate + 0 triple)))


(define (unique-triples-of-sum n s)
  (filter (lambda (triple) (check-sum triple s)) (unique-triples n)))

(unique-triples 6)

(unique-triples-of-sum 6 10)