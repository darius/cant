(load "loadme.scm")

(define (shout filename)
  (newline)
  (display "TESTING ")
  (display filename)
  (newline)
  (run-load filename))

(shout "eg/failing.scm")
(shout "eg/intset.scm")
(shout "eg/lambdacompiler.scm")
(shout "eg/lambdaterp.scm")
;(shout "eg/compact-lambda.scm")
(shout "eg/parson.scm")

(shout "eg/circuitoptimizer.scm")
(shout "eg/parse.scm")
