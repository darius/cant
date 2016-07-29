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
(struct Make (clauses)   #:transparent)
(struct Do (e1 e2)       #:transparent)
(struct Let (p e)        #:transparent)
(struct Call (e1 e2)     #:transparent)
(struct Term (tag es)    #:transparent)

;; Patterns
(struct Constant-pat (value)   #:transparent)  ; (replaceable using View-pat and Term-pat, but that'd be a pain to have come up a lot)
(struct Variable-pat (name)    #:transparent)
(struct Term-pat (tag p-args)  #:transparent)
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

(define (eval e r)
  (match e
    ({constant value}
     value)
    ({variable name}
     (r name))
    ({make e-stamp e-trait clauses}
     (actor<- (script<- (eval e-stamp (parent-only r))
                        (eval e-trait (parent-only r))
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
                   (eval e1 r))))
    ;; XXX make-trait?
    ))

(define (eval-match subject p r)
  (match p
    ({constant-pat value}
     (= subject value))
    ({variable-pat name}
     (r .bind name subject)
     #yes)
    ({term-pat tag p-args}
     (and (term? subject)
          (= subject.tag tag) ;; N.B. it'd be nice for efficiency to bundle the arity in at desugar time
          (begin matching ((args subject.arguments)
                           (p-args p-args))
            (case (args.empty? p-args.empty?)
                  (p-args.empty? #no)
                  (else (and (eval-match args.first p-args.first r)
                             (matching args.rest p-args.rest)))))))
    ({view-pat e p1}
     (eval-match (call (eval e (parent-only r))
                       `(,subject))  ;;XXX or just subject?
                 p1 r))))

;; N.B. the parent-only expressions are assumed not to define anything
;; (which should be statically checked also)

(define (exp-vars-defined e)
  (match e
    ({constant _}  '())
    ({variable _}  '())
    ({make _ _ _}  '())
    ({do e1 e2}    (chain (exp-vars-defined e1)
                          (exp-vars-defined e2)))
    ({let p _}     (pat-vars-defined p))
    ({call e1 e2}  (chain (exp-vars-defined e1)
                          (exp-vars-defined e2)))
    ({term tag es} (gather exp-vars-defined es))))

(define (pat-vars-defined p)
  (match p
    ({constant-pat _}      '())
    ({variable-pat name}   `(,name))
    ({term-pat tag p-args} (gather pat-vars-defined p-args))
    ({view-pat e p1}       (pat-vars-defined p1))))
