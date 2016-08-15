;; An error handler that prints a (crude) traceback.

;; Install this via 
;; (the-signal-handler-box .^= on-error-traceback)
(define (on-error-traceback k plaint @values)
  (print-error-traceback k plaint values))

(define (print-error-traceback k plaint values)
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

(export on-error-traceback print-error-traceback print-plaint print-traceback)
