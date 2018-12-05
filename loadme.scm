;; The Squeam interpreter and global environment.

(load "terp/util.scm")
(load "terp/macros.scm")
(load "terp/read.scm")
(load "terp/parse.scm")
(load "terp/elaborate.scm")
(load "terp/primitives.scm")
(load "terp/terp.scm")

(run-load "lib/stdlib.scm")
(squeam-interpret
 '(do
    (use "test/smoke-test")
    (import (use "lib/flexarray")  flexarray<-)
    (import (use "lib/sort")       sort)
    (import (use "lib/hashset")    set<-)
    (import (use "lib/bag")        bag<-)
    ;; Let's default to traceback-on-error:
    (push-signal-handler ((use "lib/traceback") 'on-error-traceback))))

(define (repl . opt-args)
  (squeam-interpret `(call repl ',opt-args)))
;(repl)
