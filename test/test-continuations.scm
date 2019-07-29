;; Get a continuation from an error signal, and invoke it.

(import (use 'traceback) on-error-traceback)

(with-signal-handler
 (given (k plaint @values)
   (call on-error-traceback `(,k ,plaint ,@values))
   (display "Now continuing with 42\n")
   (k .answer 42))
 (given ()
   (print (+ 1 (error "I wish to complain" 'life 'is 'terrible)))))
