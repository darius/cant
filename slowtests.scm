(load "loadme.scm")

;; Dependencies
(loud-load "eg/hashmap.scm")
(loud-load "eg/fillvector.scm")
(loud-load "eg/format.scm")
(loud-load "eg/sort.scm")
(loud-load "eg/bdd.scm")

;; Actual test
(loud-load "eg/nqueens.scm")
(interpret '(queens 4))
