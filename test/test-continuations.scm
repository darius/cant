;; Get a continuation from an error signal, and invoke it.

(import (use "lib/traceback") on-error-traceback)

(to (on-error-have-fun k plaint @values)
  (the-signal-handler-box .^= on-error-traceback) ;; Restore the usual handler.
  (call on-error-traceback `(,k ,plaint ,@values))
  (display "Now continuing with 42") (newline)
  (k .answer 42))

(the-signal-handler-box .^= on-error-have-fun)
(print (+ 1 (error "I wish to complain" 'life 'is 'terrible)))
