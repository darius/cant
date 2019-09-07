#!chezscheme
(library (player abcs)
(export load-abcs cant-interpret)
(import (chezscheme)
  (player util)
  (player read)
  (player parse)
  (player setting)
  (player primordia)
  (player player))

(define repl-env #f)                    ;XXX

(define (run-load filename)
  (let ((forms (snarf filename cant-read)))
    (cant-interpret `(do ,@forms))))

;; TODO add setting & optional context
(define (cant-interpret e)
  (evaluate (parse-exp e) repl-env))

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

;; XXX total hack
(set! repl-env (setting-extend-mutable primordial-setting)) ;TODO not really

(setting-ensure-bound repl-env '(
                                 full-powered-setting 
                                 main-interactive-setting))
(setting-resolve! repl-env 'full-powered-setting repl-env)
(setting-resolve! repl-env 'main-interactive-setting repl-env)

)
