;; The Squeam interpreter and global environment.

(load "util.scm")
(load "macros.scm")
(load "read.scm")
(load "parse.scm")
(load "terp.scm")

(run-load "lib/stdlib.scm")
(squeam-interpret
 '(do (import (use "lib/format")     format)
      (import (use "lib/fillvector") fillvector<-)
      (import (use "lib/sort")       sort)
      (import (use "lib/hashset")    set<-)
      (import (use "lib/squeam-ast") unparse-exp unparse-pat unparse-clause)
      ;; Let's default to traceback-on-error:
      (the-signal-handler .^= ((use "lib/traceback") 'on-error-traceback))))

(define (repl)
  (squeam-interpret '(repl)))
;(repl)
