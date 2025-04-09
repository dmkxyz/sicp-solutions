#lang sicp

(define (make-account balance password)
  (let ((incorrect-password-count 0))
   (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient balance"))

  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)

  (define (dispatch pwd op)
    (cond ((and (eq? pwd password) (eq? op 'withdraw)) (begin (set! incorrect-password-count 0) withdraw))
          ((and (eq? pwd password) (eq? op 'deposit)) (begin (set! incorrect-password-count 0) deposit))
          ((not (eq? pwd password)) (lambda (x)
                                      (begin (set! incorrect-password-count (+ incorrect-password-count 1))
                                             (if (> incorrect-password-count 7)
                                                 "call-the-cops"
                                                 "Incorrect password"))))
          (else (error "Unknown request -- MAKE-ACCOUNT" op))))

  dispatch))


        