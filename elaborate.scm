(include "gambit-macros.scm")

(define (elaborate e)
  (cond ((symbol? e) e)
        ((self-evaluating? e) `',e)
        ((not (pair? e)) (error "Bad syntax" e))
        ((look-up-macro (car e))
         => (lambda (expand) (elaborate (expand e))))
        ((look-up-core-syntax (car e))
         => (lambda (expand) (expand e)))
        (else (elaborate-call e))))

(define (self-evaluating? x)
  (or (boolean? x)
      (number? x)
      (char? x)
      (string? x)))

(define (elaborate-call e)
  (mcase e
    (((: cue cue?) . es)
     (cons cue (map elaborate es)))
    (_ (cons '.run (map elaborate e)))))
  
(define (look-up-core-syntax key)
  (mcase key
    ('quote  (mlambda
              ((_ datum) `',datum)))
    ('make   (mlambda
              ((_ (: name symbol?) . clauses)
               (elaborate `(hide
                            (let ,name (make . ,clauses))
                            ,name)))
              ((_ . clauses)
               `(make ,@(map elaborate-method/matcher clauses)))))
    ('hide   (mlambda
              ((_ . body)
               (elaborate-hide body))))
    ('begin  (mlambda
              ((_ e) (elaborate e))
              ((_ e . es) `(%let _ ,(elaborate e)
                             ,(elaborate `(begin . ,es))))))
    (_ #f)))

(define (elaborate-method/matcher clause)
  (define (param-list? x)
    (and (list? x) (all symbol? x)))  ;XXX and distinct
  (mcase clause
    ((cue params . body)
     (assert (not (null? body)) "Empty body" clause)
     (cond ((eq? cue 'else)
            (assert (param-list? params) "Bad param list" clause))
           (else
            (assert (cue? cue) "Not a method decl" clause)
            (assert (or (symbol? params) (param-list? params))
                    "Bad params syntax" clause)))
     `(,cue ,params ,(elaborate-hide body)))))

(define (elaborate-top-level body)
  (let ((commands (elaborate-commands body '())))
    (foldr (lambda (cmd rest)
             (mcase cmd
               (('let v e) `(%let ,v ,e ,rest))))
           ''#f
           commands)))

(define (elaborate-hide body)
  (let* ((commands (elaborate-commands body '()))
         (vars (flatmap get-hide-vars commands))
         (result (foldr (lambda (cmd rest)
                          (mcase cmd
                            (('let v e) `(%let ,v ,e ,rest))))
                        (mcase (last commands)
                          (('let '_ e) e))
                        (butlast commands))))
    (if (null? vars)
        result
        `(%hide ,vars ,result))))

(define (elaborate-commands cmds rest)
  (foldr (lambda (cmd rest)
           (mcase cmd
             (('begin . cmds1)
              (elaborate-commands cmds1 rest))
             (('include filename)
              (elaborate-commands (snarf filename) rest))
             (('make (: name symbol?) . clauses)
              `((let ,name ,(elaborate `(make . ,clauses)))
                (let _ ,name)   ; make has a value, not just a binding
                . ,rest))
             (_
              (cons (elaborate-command cmd) rest))))
         rest
         cmds))

(define (elaborate-command cmd)
  (mcase cmd
    (('let (: v symbol?) e)
     `(let ,v ,(elaborate e)))
    (('define ((: v symbol?) . params) . body)
     `(let ,v ,(elaborate `(given ,params . ,body))))
    (_
     `(let _ ,(elaborate cmd)))))

(define (get-hide-vars cmd)
  (mcase cmd
    (('let '_ e) '())
    (('let v e) (list v))))

(define (look-up-macro key)
  (mcase key
    ('given (mlambda
              ((_ vars . body)
               `(make (.run ,vars . ,body)))))
    ('with (mlambda
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
                `((hide (let ,proc (given ,(map car bindings) . ,body))
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
               `(with ((,test-var ,e1))
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
             `(with ((,t ,e)) (if ,t ,t (begin . ,es)))))))
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
