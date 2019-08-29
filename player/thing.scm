(library (player thing)
(export object? object<- object-script object-datum
        script? script<- script-name script-trait script-clauses
        uninitialized
        )
(import (chezscheme) (player util) (player env))

;; Basic data model

(define-record-type object (fields script datum))   ; Nonprimitive objects, that is.
(define object<- make-object)

(define-record-type script (fields name trait clauses))
(define script<- make-script)

(define uninitialized
  (object<- (script<- '<uninitialized> #f '()) '*uninitialized*))


)
