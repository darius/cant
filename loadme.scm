(define (report x)
  (write x)
  (newline))

(load "util.scm")
(load "read.scm")
(load "parse.scm")
(load "terp.scm")

(run-load "lib/stdlib.scm")
(run-load "lib/traceback.scm")  ;; Let's default to traceback-on-error

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
