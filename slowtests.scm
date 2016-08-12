(load "loadme.scm")

;; Dependencies
(run-load "eg/hashmap.scm")
(run-load "eg/fillvector.scm")
(run-load "eg/format.scm")
(run-load "eg/sort.scm")
(run-load "eg/bdd.scm")

;; Actual test
(run-load "eg/nqueens.scm")
(interpret '(queens 4))
