;; Kernel Squeam

;; Variable names:
;;   e expression
;;   p pattern
;;   r environment


;; Expressions
(struct Constant (value) #:transparent)
(struct Variable (name)  #:transparent)
(struct Make (clauses)   #:transparent)
(struct Do (e1 e2)       #:transparent) ; (I'm assuming case-sensitive symbols)
(struct Let (p e)        #:transparent)
(struct Call (e1 e2)     #:transparent)
(struct List (es)        #:transparent)
(struct Term (tag e)     #:transparent)

;; Patterns
(struct Constant-pat (value)   #:transparent)
(struct Any-pat ()             #:transparent)
(struct Variable-pat (name)    #:transparent)
(struct Count-pat (n)          #:transparent)
(struct Prefix-pat (ps p-rest) #:transparent)
(struct Term-pat (tag p-args)  #:transparent)
(struct And-pat (p1 p2)        #:transparent)
(struct View-pat (e p)         #:transparent)

;; Runtime data types
(struct Term-data (tag arguments) #:transparent)

(define squeam-list? list?) ; (an oversimplification)


;; Environments

;; Return a new environment extending r with unbound placeholders for names.
(define (env-extend r names) ...)

;; Mutate r to bind name to value (but raise an error unless name is
;; bound only to a placeholder).
(define (env-bind! r name value) ...)

;; Return the value r has for name. (Raise an error if that's a placeholder.)
(define (env-lookup r name) ...)


;; An object is represented as a Scheme function taking a message.
;; (This doesn't quite reflect the full language, because for a couple
;; of operations we'll want to look at the object's type -- e.g. for
;; squeam-list? above. I haven't yet figured out how I want that to work.)

(define (call actor message)
  (actor message))

(define (actor<- script r)
  (lambda (message)
     (script message r)))

(define (script<- clauses)
  (lambda (message parent-r}
    (let matching [(clauses clauses)]
      (match clauses
        [`((,pattern ,body) ,@rest)
         (let ([r (env-extend parent-r (append (pat-vars-defined pattern)
                                               (exp-vars-defined body)))])
           (if (eval-match message pattern r)
               (eval-exp body r)
               (matching rest)))]))))


;; The evaluator

(define (eval-exp e r)
  (match e
    [(Constant value)
     value]
    [(Variable name)
     (env-lookup r name)]
    [(Make clauses)
     (actor<- (script<- clauses) r)]
    [(Do e1 e2)
     (eval-exp e1 new-r)
     (eval-exp e2 new-r)]
    [(Let p e1)
     (let ([value (eval-exp e1 r)])
       (if (eval-match value p r)
           value
           (error "Match failure" p value)))]
    [(Call e1 e2)
     (call (eval-exp e1 r) (eval-exp e2 r))]
    [(List es)
     (map (lambda (e1) (eval-exp e1 r))
          es)]
    [(Term tag e1)
     (Term-data tag (eval-exp e1 r))]))

;; Return whether subject matches p; fill in r's placeholders along
;; the way. (Match failure may leave r only partly filled in.)
(define (eval-match subject p r)
  (match p
    [(Any-pat)
     #t]
    [(Variable-pat name)
     (env-bind! r name subject)
     #t]
    [(Constant-pat value)
     (equal? subject value)]  ;; N.B. equality on 'objects' is by identity only
    [(Count-pat n)
     (and (squeam-list? subject)
          (= (call subject (Term-data 'count '()))  ;; ask subject for its size
             n))]
    [(Prefix-pat ps p-rest)
     (and (squeam-list? subject)
          (match-prefix subject ps p-rest r))]
    [(Term-pat tag p-args)
     (match subject
       [(Term-data subject-tag subject-arguments)
        (and (eval-match subject-tag tag r)
             (eval-match subject-arguments p-args r))]
       [else #f])]
    [(And-pat p1 p2)
     (and (eval-match subject p1 r)
          (eval-match subject p2 r))]
    [(View-pat e p1)
     (eval-match (call (eval-exp e r) (list subject))
                 p1
                 r)]))
