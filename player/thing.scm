;; Basic data model
;; TODO explain more

(library (player thing)
(export object? object<- object-script object-datum
        script? script<- script-name script-trait script-clauses
        cps-script? cps-prim<- cps-script-name cps-script-procedure
        uninitialized
        )
(import (chezscheme) (player util))

(define-record-type object (fields script datum))   ; Nonprimitive objects, that is.
(define object<- make-object)

(define-record-type script (fields name trait clauses))
(define script<- make-script)

(define-record-type cps-script (fields name procedure))

(define (cps-prim<- datum name procedure)
  (object<- (make-cps-script name procedure) datum))

(define uninitialized
  (object<- (script<- '<uninitialized> #f '()) '*uninitialized*))


)
