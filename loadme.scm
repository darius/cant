(define (report x)
  (write x)
  (newline))

(load "util.scm")
(load "read.scm")
(load "parse.scm")
(load "terp.scm")

(run-load "stdlib.scm")
(run-load "eg/traceback.scm")  ;; Let's default to traceback-on-error

(define (repl)
  (interpret '(repl)))
;(repl)
