(define (report x)
  (write x)
  (newline))

(load "util.scm")
(load "read.scm")
(load "parse.scm")
(load "terp.scm")

(run-load "lib/stdlib.scm")
(interpret
 '(hide ;; Let's default to traceback-on-error.
   (import (use "lib/traceback.scm") on-error-traceback)
   (the-signal-handler-box .^= on-error-traceback)))

(define (loud-load filename)
  (newline)
  (display "-------- ")
  (display filename)
  (display " --------")
  (newline)
  (run-load filename))

(define (repl)
  (interpret '(repl)))
;(repl)
