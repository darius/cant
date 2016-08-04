;; Like elaborate.scm for the new scheme

(include "gambit-macros.scm")

(define (parse-exp e)
  (cond
    ((and (pair? e) (look-up-macro (car e)))
     => (lambda (expand) (parse-exp (expand e))))
    (else
     (mcase e
       ((: _ symbol?)
        (term<- 'variable e))
       (('quote datum)
        (term<- 'constant datum))
       ((: _ self-evaluating?)
        (term<- 'constant e))
       ((: _ term?)
        (term<- 'term
                (term-tag e)
                (map parse-exp (term-parts e))))
       (('let p e1)
        (term<- 'let (parse-pat p) (parse-exp e1)))
       (('make '_ . clauses)
        (parse-make #f clauses))
       (('make (: name symbol?) . clauses) ;TODO: cons up a fully-qualified name
        (term<- 'let name (parse-make name clauses)))
       (('make . clauses)
        (parse-make #f clauses))
       (('do e1)
        (parse-exp e1))
       (('do e1 . es)
        (term<- 'do (parse-exp e1) (parse-exp `(do ,@es))))
       ((addressee (: cue cue?) . operands)
        (term<- 'call
                (parse-exp addressee)
                (term<- 'term cue (map parse-exp operands))))
       ((addressee . operands)
        (term<- 'call
                (parse-exp addressee)
                (build-list (map parse-exp operands))))
       ((: _ term?)
        (term<-list `(term
                      ,(term-tag e)
                      ,@(map parse-exp (term-parts e)))))
       ))))

(define (build-list es)
  (if (null? es)
      (term<- 'constant '())
      (term<- 'term 'cons (list (car es) (build-list (cdr es))))))

;; what's the syntax for a macro in pattern context?
(define (parse-pat p)
  (mcase p
    ('_
     (term<- 'any-pat))
    ((: _ symbol?)
     (term<- 'variable-pat p))
    ((: _ self-evaluating?)
     (term<- 'constant-pat p))
    (('quote datum)
     (term<- 'constant-pat datum))
    ((': e)
     (term<- 'view-pat (parse-exp e) (term<- 'constant-pat #t)))
    ((': p1 e)
     (term<- 'and-pat (parse-pat p1)
              (term<- 'view-pat (parse-exp e) (term<- 'constant-pat #t))))
    (('@ _)                      ;XXX make @vars be some disjoint type
     (error "An @-pattern must be at the end of a list" p))
    ((: _ list?)
     (parse-list-pat p))
    ((: _ term?)
     (term<- 'term-pat (term-tag p) (map parse-pat (term-parts p))))
    ))

(define (parse-list-pat ps)
  (mcase ps
    (()
     (term<- 'constant-pat '()))
    ((('@ v))
     (parse-pat v))
    ((head . tail)
     (term<- 'view-pat
             (term<- 'variable '__as-cons)
             (term<- 'term-pat 'cons (list (parse-pat head)
                                           (parse-list-pat tail)))))))

(define (self-evaluating? x)
  (or (boolean? x)
      (number? x)
      (char? x)
      (string? x)))

(define (parse-make opt-name clauses)
  (term<- 'make opt-name (map parse-clause clauses)))

(define (parse-clause clause)
  (mcase clause
    ((pat . body)
     `(,(parse-pat pat) ,(parse-exp `(do ,@body))))))

(define (look-up-macro key)
  (mcase key
    ('hide   (mlambda
              ((_ . es)
               `((given () ,@es)))))
    ('include (mlambda             ;temporary
               ((_ (: filename string?))
                `(do ,@(snarf filename squeam-read)))))
    ('make-trait
             (mlambda
              ((_ (: v symbol?) (: self symbol?) . clauses)
               (let ((msg (gensym)))
                 `(define (,v ,self ,msg)
                    (match ,msg ,@clauses))))))
    ('match  (mlambda
              ((_ subject . clauses)
               `(call (make _ ,@clauses) ,subject))))
    ('define (mlambda
              ((_ ((: v symbol?) . params) . body)
               `(make ,v (,params ,@body)))
              ((_ (call-form . params) . body)
               `(define ,call-form (given ,params ,@body)))))
    ('given  (mlambda
              ((_ vars . body)
               `(make (,vars ,@body)))))
    ('with   (mlambda
              ((_ bindings . body)
               (parse-bindings bindings
                 (lambda (ps es)
                   `((given ,ps ,@body) ,@es))))))
    ('for    (mlambda
              ((_ fn bindings . body)
               (parse-bindings bindings
                 (lambda (ps es)
                   `(,fn (given ,ps ,@body) ,@es))))))
    ('begin  (mlambda
              ((_ (: proc symbol?) bindings . body)
               (parse-bindings bindings
                 (lambda (ps es)
                   `((hide (define (,proc ,@ps) ,@body))
                     ,@es))))))
    ('if     (mlambda
              ((_ test if-so if-not)
               `(match ,test
                  (#f ,if-not)
                  (_ ,if-so)))))
    ('when   (mlambda
              ((_ test . body)
               `(if ,test (do ,@body) #f))))
    ('unless (mlambda
              ((_ test . body)
               `(if ,test #f (do ,@body)))))
    ('case   (mlambda
              ((_) #f)                 ;TODO: generate an error-raising?
              ((_ ('else . es)) `(do ,@es))
              ((_ (e) . clauses) `(or ,e (case ,@clauses)))
              ((_ (e1 '=> e2) . clauses)
               (let ((test-var (gensym)))
                 `(with ((,test-var ,e1))
                    (if ,test-var
                        (,e2 ,test-var)
                        (case ,@clauses)))))
              ((_ (e . es) . clauses)
               `(if ,e (do ,@es) (case ,@clauses)))))
    ('and    (mlambda
              ((_) #t)
              ((_ e) e)
              ((_ e . es) `(if ,e (do ,@es) #f))))
    ('or     (mlambda
              ((_) #f)
              ((_ e) e)
              ((_ e . es)
               (let ((t (gensym)))
                 `(with ((,t ,e)) (if ,t ,t (do ,@es)))))))
    ('quasiquote (mlambda
                  ((_ q) (expand-quasiquote q))))
    (_ #f)))

(define (parse-bindings bindings receiver)
  (for-each (lambda (binding)
              (mcase binding
                (((: _ symbol?) _)
                 'ok)))
            bindings)
  (receiver (map car bindings) (map cadr bindings)))

(define (expand-quasiquote e)
  (mcase e
    (('unquote e1) e1)
    ((('unquote-splicing e1)) e1)
    ((('unquote-splicing e1) . qcdr)
     `(,e1 .chain ,(expand-quasiquote qcdr))) ;XXX use a global fn instead?
    ((qcar . qcdr)
     (qq-cons e (expand-quasiquote qcar)
                (expand-quasiquote qcdr)))
    (else `',e)))

(define (qq-cons pair qq-car qq-cdr)
  (mcase `(,qq-car ,qq-cdr)
    ((('quote a) ('quote d))
     `',(reuse-cons pair a d))
    (_ `(',cons ,qq-car ,qq-cdr))))

(define (reuse-cons pair a d)
  (if (and (eqv? (car pair) a)
           (eqv? (cdr pair) d))
      pair
      (cons a d)))
