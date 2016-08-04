;; Kernel Squeam
;; Starting from new-self-terp-simplified.rkt
;; but now let's try making cons pairs be a special type of Term
;; and making list patterns be sugar for coerce-to-cons-or-nil followed by match on parts.
;; Also, tags are constants, not subpatterns.
;; (So now we need a primitive to deconstruct a term, for the variable-tag case.)
;; Let's see how we like that design.

;; Variable names:
;;   e expression
;;   p pattern
;;   r environment


;; Expressions
(struct Constant (value) #:transparent)
(struct Variable (name)  #:transparent)
(struct Make (name stamp trait clauses) #:transparent)   ; stamp and trait are variables, which must be from an enclosing scope
(struct Do (e1 e2)       #:transparent)
(struct Let (p e)        #:transparent)
(struct Call (e1 e2)     #:transparent)
(struct Term (tag es)    #:transparent)

;; Patterns
(struct Any-pat ()             #:transparent)
(struct Variable-pat (name)    #:transparent)
(struct Constant-pat (value)   #:transparent)
(struct Term-pat (tag p-args)  #:transparent)  ; TODO consider making it like Term-pat (term)
(struct And-pat (p1 p2)        #:transparent)
(struct View-pat (e p)         #:transparent)

;; Runtime data types
(struct Term-data (tag arguments) #:transparent)

;; Now a list pat like (a b) becomes
;; (View-pat as-pair (Term-pat pair
;;                             (Variable-pat a)
;;                             (View-pat as-pair (Term-pat pair
;;                                                         (Variable-pat b)
;;                                                         (View-pat as-nil (Term-pat nil ()))))))
;; which seems horrifyingly inefficient... The old design would've had
;; (Prefix-pat ((Variable-pat a)
;;              (Variable-pat b))
;;             (Any-pat))
;; So maybe we'd best start with this new simpler design, then go back
;; to something like the old one after the system settles down more.
;; Or we could desugar common cases to something like
;; (View-pat tuple-2<-list (Term-pat tuple-2 (Variable-pat a) (Variable-pat b)))
;; -- We need a name pattern for coercions like this that are allowed to fail
;;    or else we need to specify how exceptions work so that pattern-matching
;;    can use them...

;; OK, so the interpreter now looks like:

(define (script<- stamp trait clauses)
  (when stamp (assert (stamp? stamp)))
;  (assert (trait? trait))
  (make script
    ({.receive message parent-r}
     (begin matching ((clauses clauses))
       (match clauses
         (()
          (delegate trait
                    (actor<- script parent-r) ;N.B. must not change the identity: = must compare script, r parts
                    message))
         (((pattern body) @rest)
          (let pat-r (env-extend parent-r (pat-vars-defined pattern)))
          (if (eval-match message pattern pat-r)
              (eval body (env-extend pat-r (exp-vars-defined body)))
              (matching rest))))))
    ({.verify alleged-stamp}
     ;;XXX this is probably crap; figure it out
     (= stamp alleged-stamp))))

(define (delegate trait self message)
  (trait self message))

(define (actor<- script r)
  ;; XXX This is not quite right: we need to virtualize the stamp stuff too.
  (make _
    (message
     (script .receive message r))))

(define (eval e r)
  (match e
    ({constant value}
     value)
    ({variable name}
     (r name))
    ({make _ stamp trait clauses}
     (actor<- (script<- (eval stamp r)
                        (eval trait r)
                        clauses)
              r))
    ({do e1 e2}
     (eval e1 r)
     (eval e2 r))
    ({let p e1}
     (let value (eval e1 r))
     (if (eval-match value p r)
         value
         (error "Match failure" p value)))
    ({call e1 e2}
     (call (eval e1 r) (eval e2 r)))
    ({term tag es}
     (term<- tag (for each ((e1 es))
                   (eval e1 r))))))

(define (eval-match subject p r)
  (match p
    ({any-pat}
     #yes)
    ({variable-pat name}
     (r .bind name subject)
     #yes)
    ({constant-pat value}
     (= subject value))
    ({term-pat tag p-args}
     (and (term? subject)
          (= subject.tag tag) ;; N.B. it'd be nice for efficiency to bundle the arity in at desugar time
          (match-all subject.arguments p-args r)))
    ({and-pat p1 p2}
     (and (eval-match subject p1 r)
          (eval-match subject p2 r)))
    ({view-pat e p}
     (eval-match (call (eval e r) `(,subject))
                 p r))))

(define (match-all values pats r)
  (case (values.empty? pats.empty?)
        (pats.empty? #no)
        (else (and (eval-match values.first pats.first r)
                   (match-all  values.rest  pats.rest  r)))))

(define (exp-vars-defined e)
  (match e
    ({constant _}  '())
    ({variable _}  '())
    ({make _ _ _}  '())
    ({do e1 e2}    (chain (exp-vars-defined e1)
                          (exp-vars-defined e2)))
    ({let p e}     (chain (pat-vars-defined p)
                          (exp-vars-defined e)))
    ({call e1 e2}  (chain (exp-vars-defined e1)
                          (exp-vars-defined e2)))
    ({term tag es} (gather exp-vars-defined es))))

(define (pat-vars-defined p)
  (match p
    ({any-pat}             '())
    ({variable-pat name}   `(,name))
    ({constant-pat _}      '())
    ({term-pat tag p-args} (gather pat-vars-defined p-args))
    ({and-pat p1 p2}       (chain (pat-vars-defined p1)
                                  (pat-vars-defined p2)))
    ({view-pat e p}        (chain (exp-vars-defined e)
                                  (pat-vars-defined p)))))
