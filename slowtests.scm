(load "loadme.scm")

;; Dependencies
(loud-load "lib/hashmap.scm")
(loud-load "lib/fillvector.scm")
(loud-load "lib/format.scm")
(loud-load "lib/sort.scm")
(loud-load "lib/bdd.scm")

;; Actual test
(loud-load "eg/nqueens.scm")
(interpret '(queens 4))

(report-stats)
