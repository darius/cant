;; The Squeam interpreter and global environment.

(load "util.scm")
(load "macros.scm")
(load "read.scm")
(load "parse.scm")
(load "terp.scm")

(run-load "lib/stdlib.scm")
(squeam-interpret
 '(do (import (use "lib/hashmap")
              map<-a-list  ; Needed globally for (export ...) to work.
              map<-)       ; Just convenient to make global.
      ;; More global conveniences:
      (import (use "lib/format")     format)
      (import (use "lib/fillvector") fillvector<-)
      (import (use "lib/sort")       sort)
      (import (use "lib/hashset")    set<-)
      ))
(squeam-interpret
 '(hide ;; Let's default to traceback-on-error.
   (import (use "lib/traceback") on-error-traceback)
   (the-signal-handler-box .^= on-error-traceback)))

(define (repl)
  (squeam-interpret '(repl)))
;(repl)
