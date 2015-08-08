(load "terp.scm")

(define (shout filename)
  (newline)
  (display "TESTING ")
  (display filename)
  (newline)
  (run-load filename))

(shout "intset.scm")
(shout "lambdacompiler.scm")
(shout "lambdaterp.scm")
(shout "parson.scm")

(shout "circuitoptimizer.scm")
