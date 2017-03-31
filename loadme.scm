;; The Squeam interpreter and global environment.

(load "terp/util.scm")
(load "terp/macros.scm")
(load "terp/read.scm")
(load "terp/parse.scm")
(load "terp/terp.scm")

(run-load "lib/stdlib.scm")
(squeam-interpret
 '(do
    ;; unparse-foo are used by runtime for tracebacks:
    (import (use "lib/squeam-ast") unparse-exp unparse-pat unparse-clause)
    (import (use "lib/format")     format)
    (import (use "lib/fillvector") fillvector<-)
    (import (use "lib/sort")       sort)
    (import (use "lib/hashset")    set<-)
    ;; Let's default to traceback-on-error:
    (the-signal-handler .^= ((use "lib/traceback") 'on-error-traceback))))

(define (repl)
  (squeam-interpret '(repl)))
;(repl)
