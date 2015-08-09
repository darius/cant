;(load "terp.scm")
(load "loadme.scm")

(define (shout filename)
  (newline)
  (display "TESTING ")
  (display filename)
  (newline)
  (run-load filename))

(shout "failing.scm")
(shout "intset.scm")
(shout "lambdacompiler.scm")
(shout "lambdaterp.scm")
(shout "parson.scm")

(shout "circuitoptimizer.scm")
