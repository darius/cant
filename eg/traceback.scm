;; Install an error handler that prints a (crude) traceback.

(define (on-error-traceback k plaint @values)
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

(the-signal-handler-box .^= on-error-traceback)
