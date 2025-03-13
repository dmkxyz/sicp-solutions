#lang sicp
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      nil
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))

(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (lambda (u) (dot-product v u)) m))

(define (transpose mat)
  (accumulate-n cons nil mat))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (u) (matrix-*-vector cols u)) m)))

(define mat '((1 2 3) (4 5 6) (7 8 9)))

(define identity '((1 0 0) (0 1 0) (0 0 1)))

(matrix-*-vector mat (car identity))
(map car mat)

(matrix-*-matrix mat identity)
mat

(matrix-*-matrix identity mat)
mat