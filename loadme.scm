;; The Squeam interpreter and global environment.

(load "terp/util.scm")
(load "terp/macros.scm")
(load "terp/read.scm")
(load "terp/parse.scm")
(load "terp/elaborate.scm")
(load "terp/terp.scm")

(run-load "lib/stdlib.scm")
(squeam-interpret
 '(do
    ;; unparse-foo are used by runtime for tracebacks:
;    (import (use "lib/squeam-ast") unparse-exp unparse-pat unparse-clause)
    (use "test/smoke-test")
;    (import (use "lib/format")     format)
    (import (use "lib/flexarray")  flexarray<-)
    (import (use "lib/sort")       sort)
    (import (use "lib/hashset")    set<-)
    ;; Let's default to traceback-on-error:
    (push-signal-handler ((use "lib/traceback") 'on-error-traceback))))

(define (repl . opt-args)
  (squeam-interpret `(call repl ',opt-args)))
;(repl)
