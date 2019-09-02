(library (player elaborate)
(export elaborate)
(import (chezscheme)
  (player util) (player macros) (player ast) (player setting))

;; Analyze and transform a parsed AST.

(define (elaborate e setting)
  (let* ((defined (exp-vars-defined e))
         (scope (if (or (null? defined) (mutable-setting? setting))
                    (make-scope defined setting)
                    (make-scope '() (setting-extend-promises setting defined))))
         (e-e (elaborate-e e scope)))
    (list e-e (scope-setting scope))))

(define (elaborate-e e s)
  ((vector-ref methods/elaborate-e (pack-tag e))
   e s))

(define (elaborate-es es s)
  (map (lambda (e) (elaborate-e e s)) es))

;; TODO none of this rewriting actually changes anything right now

(define methods/elaborate-e
  (vector
   (lambda (e s)                        ;e-constant
     e)
   (lambda (e s)                        ;e-variable
     (unpack e (name)
       (unless (scope-find? s name)
         (printf "Warning: unbound variable: ~s\n" name)) ;TODO don't repeat the same warning
       e))
   (lambda (e s)                        ;e-term
     (unpack e (tag args)
       (pack<- e-term tag (elaborate-es args s))))
   (lambda (e s)                        ;e-list
     (unpack e (args)
       (pack<- e-list (elaborate-es args s))))
   (lambda (e s)                        ;e-make
     (unpack e (name trait clauses)
       (pack<- e-make name (elaborate-e trait s)
               (map (lambda (clause) (elaborate-clause clause s))
                    clauses))))
   (lambda (e s)                        ;e-do
     (unpack e (e1 e2)
       (pack<- e-do (elaborate-e e1 s)
                    (elaborate-e e2 s))))
   (lambda (e s)                        ;e-let
     (unpack e (p1 e1)
       (pack<- e-let (elaborate-p p1 s)
                     (elaborate-e e1 s))))
   (lambda (e s)                        ;e-call
     (unpack e (e1 e2)
       (pack<- e-call (elaborate-e e1 s)
                      (elaborate-e e2 s))))))

(define (elaborate-clause clause s)
  (mcase clause
    ((p pvs evs e)
     (let ((vs (append pvs evs)))
       (check-for-duplicates vs)
       (let ((nest-s (scope-extend s vs)))
         `(,(elaborate-p p nest-s)
           ,pvs
           ,evs
           ,(elaborate-e e nest-s)))))))

(define (check-for-duplicates vars)
  'ok)                                  ;TODO

(define (elaborate-p p s)
  ((vector-ref methods/elaborate-p (pack-tag p))
   p s))

(define (elaborate-ps ps s)
  (map (lambda (p) (elaborate-p p s)) ps))

(define methods/elaborate-p
  (vector
   (lambda (p s)                        ;p-constant
     p)
   (lambda (p s)                        ;p-any
     p)
   (lambda (p s)                        ;p-variable
     p)
   (lambda (p s)                        ;p-term
     (unpack p (tag args)
       (pack<- p-term tag (elaborate-ps args s))))
   (lambda (p s)                        ;p-list
     (unpack p (args)
       (pack<- p-list (elaborate-ps args s))))
   (lambda (p s)                        ;p-and
     (unpack p (p1 p2)
       (pack<- p-and (elaborate-p p1 s)
                     (elaborate-p p2 s))))
   (lambda (p s)                        ;p-view
     (unpack p (e1 p1)
       (pack<- p-view (elaborate-e e1 s)
                      (elaborate-p p1 s))))))

(define-record-type scope (fields vars setting))

(define (scope-find? scope v)
  (or (not (not (memq v (scope-vars scope))))
      (setting-binds? (scope-setting scope) v)))

(define (scope-extend scope vars)
  (make-scope (append vars (scope-vars scope))
              (scope-setting scope)))

)
