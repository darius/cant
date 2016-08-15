;; The Squeam interpreter and global environment.

(load "util.scm")
(load "read.scm")
(load "parse.scm")
(load "terp.scm")

(run-load "lib/stdlib.scm")
(interpret
 '(do (import (use "lib/hashmap.scm")
              map<-a-list  ; Needed globally for (export ...) to work.
              map<-)       ; Just convenient to make global.
      ;; More global conveniences:
      (import (use "lib/format.scm")     format)
      (import (use "lib/fillvector.scm") fillvector<-)
      (import (use "lib/sort.scm")       sort)
      (import (use "lib/hashset.scm")    set<-)
      ))
(interpret
 '(hide ;; Let's default to traceback-on-error.
   (import (use "lib/traceback.scm") on-error-traceback)
   (the-signal-handler-box .^= on-error-traceback)))

(define (repl)
  (interpret '(repl)))
;(repl)
