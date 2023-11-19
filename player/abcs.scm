#!chezscheme

;; Bring up the system with the rest of the runtime written in Cant,
;; loading it into primordial-setting.
;; (We assume abcs/00-primordia is already loaded.)

(library (player abcs)
(export load-abcs cant-interpret)
(import (chezscheme)
  (player util)
  (player read)
  (player parse)
  (player setting)
  (player primordia)
  (player player))

(define base-path (getenv "CANT_DIR"))

(define (run-load filename)
  (unless base-path
    (error 'run-load "CANT_DIR not set"))
  (let ((forms (snarf (string-append base-path "/" filename) cant-read)))
    (cant-interpret `(so ,@forms))))

;; TODO add setting & optional context
(define (cant-interpret e)
  (evaluate (parse-exp e) primordial-setting))

(define (load-abcs)
  (run-load "abcs/20-cant.cant")
  (run-load "abcs/21-sequels.cant")
  (run-load "abcs/30-functions.cant")
  (run-load "abcs/40-library.cant")
  (run-load "abcs/50-top.cant")
  (cant-interpret
   '(so
      ;;   (use "test/smoke-test")
      (the-last-oops .^))))

(setting-ensure-bound primordial-setting '(primordial-setting))
(setting-resolve! primordial-setting 'primordial-setting primordial-setting)

)
