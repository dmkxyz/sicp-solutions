#lang sicp

(define (make-account balance password)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient balance"))

  (define (deposit amount)
    (set! balance (+ balance amount)))

  (define (dispatch pwd op)
    (cond ((and (eq? pwd password) (eq? op 'withdraw)) withdraw)
          ((and (eq? pwd password) (eq? op 'deposit)) deposit)
          ((not (eq? pwd password)) (lambda (x) "Incorrect password"))
          (else (error "Unknown request -- MAKE-ACCOUNT" op))))

  dispatch)


        