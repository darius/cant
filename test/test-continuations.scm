;; Get a continuation from an error signal, and invoke it.

(import (use 'traceback) on-error-traceback)

(with-signal-handler
 (on (k evil)
   (on-error-traceback k evil)
   (display "Now continuing with 42\n")
   (k .answer 42))
 (:
   (print (+ 1 (error "I wish to complain" 'life 'is 'terrible)))))
