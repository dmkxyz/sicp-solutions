#lang sicp

(define (square x)
  (* x x))

(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0) (/ trials-passed trials))
          ((experiment)
           (iter (- trials-remaining 1) (+ trials-passed 1)))
           (else
            (iter (- trials-remaining 1) trials-passed))))
  (iter trials 0))

(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (random range))))

(define (rectangle-area x1 x2 y1 y2)
  (* (- x2 x1) (- y2 y1)))

(define (estimate-integral P x1 x2 y1 y2 trials)
  (let ((experiment (lambda ()
                      (P (random-in-range x1 x2)
                         (random-in-range y1 y2)))))
  (* (monte-carlo trials experiment)
     (rectangle-area x1 x2 y1 y2))))

(define (estimate-pi trials)
  (let ((P (lambda (x y)
             (<= (+ (square x) (square y)) 1))))
  (estimate-integral P -1.0 1.0 -1.0 1.0 trials)))

(estimate-pi 1000000)