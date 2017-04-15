;; An error handler that prints a (crude) traceback.

;; We try to call on less of the library than usual, to work better
;; when the system is borked.

(to (on-error-traceback k @evil)
  (display "Error! Traceback:\n")
  (print-traceback k)
  (call complain evil))

(to (on-error-complain k @evil)
  (display "Error!\n")
  (call complain evil))

(to (print-traceback k)
  (each! print (reverse k)))

(to (complain @evil)
  (match evil
    (`(,(? string? plaint) ,@values)
     (display plaint)
     (display ": ")
     (write values))
    (_
     (display "Nonstandard evil: ")
     (write evil)))
  (newline))

(export on-error-traceback on-error-complain complain print-traceback)
