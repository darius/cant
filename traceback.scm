;; Install an error handler that prints a (crude) traceback.

(load "stdlib.scm")

(define (on-error-traceback k plaint values)
  (print-plaint plaint values)
  (print-traceback k))

(define (print-plaint plaint values)
  (display "Error! ")
  (write plaint)
  (display ": ")
  (write values)
  (newline))

(define (print-traceback k)
  (each! print k))

(.set! the-signal-handler-box on-error-traceback)
