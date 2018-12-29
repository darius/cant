(library (terp env)
(export primitive-env repl-env
        global-defined? really-global-lookup global-init! really-global-define!
        missing
        )
(import (chezscheme) (terp util))

;; The global environment
;; This organization is a hacky breaking of a dependency cycle

(define primitive-env '())
(define repl-env '())

(define globals (make-eq-hashtable))
(define missing (list '*missing*))

(define (global-defined? v)
  ;;XXX or (not (eq? value uninitialized))
  (eq-hashtable-contains? globals v))

(define (really-global-lookup v)
  (eq-hashtable-ref globals v missing))

(define (global-init! v value)
  (eq-hashtable-set! globals v value))

(define (really-global-define! v value)
  ;;XXX as a hack, allow global redefinition for
  ;; now. This aids development at the repl, but we
  ;; need a more systematic solution.
  ;;(signal k "Global redefinition" v)
  (let ((value (eq-hashtable-ref globals v missing)))
    (unless (eq? value missing)
      (display "\nWarning: redefined ")
      (write v)
      (newline)))
  (eq-hashtable-set! globals v value))

)
