;; An error handler that prints a (crude) traceback.

;; Install this via 
;; (the-signal-handler-box .^= on-error-traceback)
(to (on-error-traceback k plaint @values)
  (print-error-traceback k plaint values))

(to (print-error-traceback k plaint values)
  (display "Error! Traceback:\n")
  (print-traceback k)
  (display plaint)
  (display ": ")
  (write values)
  (newline))

(to (print-traceback k)
  (each! print (reverse k)))

(export on-error-traceback print-error-traceback print-traceback)
