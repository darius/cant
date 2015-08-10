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

(define (elaborate-seq es)
  (elaborate (mcase es
               ((e) e)
               ((e . es) `((lambda (,(gensym)) . ,es)
                           ,e)))))

(define (look-up-macro key)
  (mcase key
    ('lambda (mlambda
              ((_ vars . body)
               `(make (.run ,vars . ,body)))))
    ('let (mlambda
           ((_ bindings . body)
            `((lambda ,(map car bindings) . ,body)
              . ,(map cadr bindings)))))
    ('recurse (mlambda
               ((_ (: proc symbol?) bindings . body)
                (for-each (mlambda (((: v symbol?) e) 'ok))
                          bindings)
                `((letrec ((,proc (lambda ,(map car bindings) . ,body)))
                    ,proc)
                  . ,(map cadr bindings)))))
    ('if (mlambda
          ((_ test if-so) `(if ,test ,if-so #f))
          ((_ test if-so if-not)
           ;; TODO: I suspect the boolean coercion is a bad idea in
           ;; our context: there's too much polymorphism.
           `(.choose (.run ',boolean<- ,test)
                     (lambda () ,if-so)
                     (lambda () ,if-not)))))
    ('when (mlambda
            ((_ test . body)
             `(if ,test (begin . ,body)))))
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
    (_ #f)))
