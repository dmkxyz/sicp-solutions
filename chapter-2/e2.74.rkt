#lang sicp
(define division car)

(define filename cadr)

(define (get-record generic-personnel-file employee-name)
  ((get 'get-record (division generic-personnel-file)) employee-name (filename generic-personnel-file)))

(define (get-salary generic-employee-record)
  ((get 'get-salary (division generic-employee-record)) (employee-record generic-employee-record)))

(define (find-employee-record employee-name generic-files)
  (if (null? generic-files)
      (error "Record not found")
      (let ((record (get-record (car generic-files) employee-name)))
        if (record)
        record
        (find-employee-record employee-name (cdr generic-files)))))
      