#!chezscheme
(library (player abcs)
(export load-abcs)
(import (chezscheme) (player player))

(define (load-abcs)
  (run-load "abcs/20-cant.cant")
  (run-load "abcs/21-sequels.cant")
  (run-load "abcs/30-functions.cant")
  (run-load "abcs/40-library.cant")
  (run-load "abcs/50-top.cant")
  (cant-interpret
   '(do
                                        ;    (use "test/smoke-test")
      ;; Let's default to traceback-on-error:
      ;; TODO: also stash the error in the-last-error for below
                                        ;    (push-signal-handler ((use 'traceback) 'on-error-traceback))
      (import (use 'flexarray)  flexarray<- flexarray<-list)
      (import (use 'sort)       sort sort-by)
      (import (use 'bag)        bag<-)
      (the-last-error .^))))

)
