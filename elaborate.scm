(include "gambit-macros.scm")

(define (elaborate e)
  (cond ((symbol? e) e)
        ((self-evaluating? e) `',e)
        ((not (pair? e)) (error "Bad syntax" e))
        ((look-up-macro (car e))
         => (lambda (expand) (elaborate (expand e))))
        ((look-up-core-syntax (car e))
         => (lambda (expand) (expand e)))
        (else (elaborate-call (car e) (cdr e)))))

(define (self-evaluating? x)
  (or (boolean? x)
      (number? x)
      (char? x)
      (string? x)))

(define (elaborate-call first rest)
  (if (cue? first)
      (cons first (map elaborate rest))
      (cons '.run (map elaborate (cons first rest)))))  ; default cue
  
(define (look-up-core-syntax key)
  (mcase key
    ('quote  (mlambda
              ((_ datum) `',datum)))
    ('make   (mlambda
              ((_ (: name symbol?) . clauses)
               (elaborate `(letrec ((,name (make . ,clauses)))
                             ,name)))
              ((_ . clauses)
               `(make ,@(map elaborate-method/matcher clauses)))))
    ('hide   (mlambda
              ((_ . body)
               (elaborate-hide body))))
    ('letrec (mlambda
              ((_ () . body)
               (elaborate-seq body))
              ((_ defns . body)
               `(letrec ,(map (mlambda (((: v symbol?) e)
                                        `(,v ,(elaborate e))))
                              defns)
                  ,(elaborate-seq body)))))
    ('begin  (mlambda                   ;XXX not really core syntax
              ((_ . es)
               (elaborate-seq es))))
    (_ #f)))

(define (elaborate-method/matcher clause)
  ;; XXX check more of the syntax
  (assert (or (cue? (car clause)) (eq? (car clause) 'else))
          "Bad method/matcher syntax" clause)
  `(,(car clause) ,(cadr clause)
    ,(elaborate-seq (cddr clause))))

(define (elaborate-hide body)
  (let* ((commands (map elaborate-command body))
         (vars (flatmap get-hide-vars commands))
         (body (foldr (lambda (cmd rest)
                        (mcase cmd
                          (('define v e) `(%define ,v ,e ,rest))))
                      (mcase (last commands)
                        (('define '_ e) e))
                      (butlast commands))))
    (if (null? vars)
        body
        `(%hide ,vars ,body))))

(define (elaborate-command cmd)
  (mcase cmd
    (('define (: v symbol?) e)
     `(define ,v ,(elaborate e)))
    (('define ((: v symbol?) . params) . body)
     `(define ,v ,(elaborate `(given ,params . ,body))))
    (_
     `(define _ ,(elaborate cmd)))))

(define (get-hide-vars cmd)
  (mcase cmd
    (('define '_ e) '())
    (('define v e) (list v))))

(define (elaborate-seq es)
  (elaborate (mcase es
               ((e) e)
               ((e . es) `((given (,(gensym)) . ,es)
                           ,e)))))

(define (look-up-macro key)
  (mcase key
    ('given (mlambda
              ((_ vars . body)
               `(make (.run ,vars . ,body)))))
    ('let (mlambda
           ((_ bindings . body)
            (check-bindings bindings)
            `((given ,(map car bindings) . ,body)
              . ,(map cadr bindings)))))
    ('for (mlambda
           ((_ fn bindings . body)
            (check-bindings bindings)
            `(,fn (given ,(map car bindings) . ,body)
                  . ,(map cadr bindings)))))
    ('recurse (mlambda
               ((_ (: proc symbol?) bindings . body)
                (check-bindings bindings)
                `((letrec ((,proc (given ,(map car bindings) . ,body)))
                    ,proc)
                  . ,(map cadr bindings)))))
    ('if (mlambda
          ((_ test if-so) `(if ,test ,if-so #f))
          ((_ test if-so if-not)
           ;; TODO: I suspect the boolean coercion is a bad idea in
           ;; our context: there's too much polymorphism.
           `(.choose (.run ',boolean<- ,test)
                     (given () ,if-so)
                     (given () ,if-not)))))
    ('when (mlambda
            ((_ test . body)
             `(if ,test (begin . ,body)))))
    ('unless (mlambda
              ((_ test . body)
               `(if ,test #f (begin . ,body)))))
    ('cond (mlambda
            ((_) #f)                 ;TODO: generate an error-raising?
            ((_ ('else . es)) `(begin . ,es))
            ((_ (e) . clauses) `(or ,e (cond . ,clauses)))
            ((_ (e1 '=> e2) . clauses)
             (let ((test-var (gensym)))
               `(let ((,test-var ,e1))
                  (if ,test-var
                      (,e2 ,test-var)
                      (cond . ,clauses)))))
            ((_ (e . es) . clauses)
             `(if ,e (begin . ,es) (cond . ,clauses)))))
    ('and (mlambda
           ((_) #t)
           ((_ e) e)
           ((_ e . es) `(if ,e (begin . ,es) #f))))
    ('or (mlambda
          ((_) #f)
          ((_ e) e)
          ((_ e . es)
           (let ((t (gensym)))
             `(let ((,t ,e)) (if ,t ,t (begin . ,es)))))))
    ('quasiquote (mlambda
                  ((_ q) (expand-quasiquote q))))
    (_ #f)))

(define (check-bindings bindings)
  (for-each (mlambda (((: v symbol?) e) 'ok))
            bindings))

(define (expand-quasiquote e)
  (mcase e
    (('unquote e1) e1)
    ((('unquote-splicing e1)) e1)
    ((('unquote-splicing e1) . qcdr)
     `(.chain ,e1 ,(expand-quasiquote qcdr)))
    ((qcar . qcdr)
     `(cons ,(expand-quasiquote qcar) ;XXX hygiene
            ,(expand-quasiquote qcdr)))
    (else `',e)))
