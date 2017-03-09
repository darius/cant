;; An error handler that prints a (crude) traceback.

;; Install this via 
;; (the-signal-handler-box .^= on-error-traceback)
(to (on-error-traceback k plaint @values)
  (print-error-traceback k plaint values))

(to (print-error-traceback k plaint values)
  (print-plaint plaint values)
  (print-traceback k))

(to (print-plaint plaint values)
  (display "Error! ")
  (write plaint)
  (display ": ")
  (write values)
  (newline))

(to (print-traceback k)
  (each! print k))

(export on-error-traceback print-error-traceback print-plaint print-traceback)
