(define (report x)
  (write x)
  (newline))

(load "util.scm")
(load "read.scm")
(load "parse.scm")
(load "terp.scm")

(run-load "stdlib.scm")

(define (repl)
  (interpret '(repl)))
