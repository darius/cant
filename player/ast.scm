#!chezscheme

;; The internal representation of Cant abstract syntax trees. The
;; player sees them as vectors with small-integer tags; the Cant user
;; will see them as wrapper objects, as implemented elsewhere.

(library (player ast)
(export pack<- pack-tag 
        e-constant
        e-variable
        e-term
        e-list
        e-make
        e-so
        e-let
        e-call
        p-constant
        p-any
        p-variable
        p-term
        p-list
        p-and
        p-view
        none-exp
        exp-vars-defined pat-vars-defined
        )
(import (chezscheme) (player util) (player macros))

;; Parse expressions and patterns to ASTs

(define pack<- vector)

(define (pack-tag vec)
  (vector-ref vec 0))

(define-enum
  e-constant
  e-variable
  e-term
  e-list
  e-make
  e-so
  e-let
  e-call)

(define-enum
  p-constant
  p-any
  p-variable
  p-term
  p-list
  p-and
  p-view)

(define none-exp (pack<- e-constant '#f))

;; Variables defined

(define (exp-vars-defined e)
  ((vector-ref methods/exp-vars-defined (pack-tag e))
   e))

(define methods/exp-vars-defined
  (vector
   (lambda (e) '())                         ;e-constant
   (lambda (e) '())                         ;e-variable
   (lambda (e)                              ;e-term
     (unpack e (tag args)
       (flatmap exp-vars-defined args)))
   (lambda (e)                              ;e-list
     (unpack e (args)
       (flatmap exp-vars-defined args)))
   (lambda (e)                              ;e-make
     '())
   (lambda (e)                              ;e-so
     (unpack e (e1 e2)
       (append (exp-vars-defined e1)
               (exp-vars-defined e2))))
   (lambda (e)                              ;e-let
     (unpack e (p1 e1)
       (append (pat-vars-defined p1)
               (exp-vars-defined e1))))
   (lambda (e)                              ;e-call
     (unpack e (e1 e2)
       (append (exp-vars-defined e1)
               (exp-vars-defined e2))))))

(define (pat-vars-defined p)
  ((vector-ref methods/pat-vars-defined (pack-tag p))
   p))

(define methods/pat-vars-defined
  (vector
   (lambda (p) '())                         ;p-constant
   (lambda (p) '())                         ;p-any
   (lambda (p)                              ;p-variable
     (unpack p (depth offset var)
       (list var)))
   (lambda (p)                              ;p-term
     (unpack p (tag args)
       (flatmap pat-vars-defined args)))
   (lambda (p)                              ;p-list
     (unpack p (args)
       (flatmap pat-vars-defined args)))
   (lambda (p)                              ;p-and
     (unpack p (p1 p2)
       (append (pat-vars-defined p1)
               (pat-vars-defined p2))))
   (lambda (p)                              ;p-view
     (unpack p (e1 p1)
       (append (exp-vars-defined e1)
               (pat-vars-defined p1))))))

)
