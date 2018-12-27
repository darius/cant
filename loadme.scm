;; The Squeam interpreter and global environment.

(load "terp/util.so")
(load "terp/macros.so")
(load "terp/read.so")
(load "terp/parse.so")
(load "terp/elaborate.so")
(load "terp/primitives.so")
(load "terp/terp.so")

(run-load "lib/stdlib.scm")
(squeam-interpret
 '(do
    (use "test/smoke-test")
    (import (use "lib/flexarray")  flexarray<-)
    (import (use "lib/sort")       sort sort-by)
    (import (use "lib/hashset")    set<-)
    (import (use "lib/bag")        bag<-)
    ;; Let's default to traceback-on-error:
    (push-signal-handler ((use "lib/traceback") 'on-error-traceback))))

(define (repl . opt-args)
  (squeam-interpret `(call repl ',opt-args)))
;(repl)
