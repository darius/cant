;; Get a continuation from an error signal, and invoke it.

(define (on-error-have-fun k plaint @values)
  (call on-error-traceback `(,k ,plaint ,@values))
  (display "Now continuing with 42") (newline)
  (k .answer 42))

(the-signal-handler-box .^= on-error-have-fun)
(print (+ 1 (error "I wish to complain" 'life 'is 'terrible)))

;; Restore the usual handler.
(the-signal-handler-box .^= on-error-traceback)
