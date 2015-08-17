(include "gambit-macros.scm")

(define (elaborate-expression e)
  (elaborate top-context e #f))

(define (elaborate-top-level commands)
  (mcase (elaborate top-context `(hide . ,commands) #f)
    (('%hide _ ast) ast)
    (ast ast)))

(define-structure context counter names)

(define top-context (make-context 0 '()))

(define (extend-context context opt-name)
  (make-context 0 (cons (or opt-name (next-anon context))
                        (context-names context))))

(define (next-anon context)
  (let ((n (+ (context-counter context) 1)))
    (context-counter-set! context n)
    (string->symbol (string-append "#" (number->string n)))))

(define (fq-name<- context)
  (string->symbol
   (string-join "." (reverse (map symbol->string (context-names context))))))

;; If e is a command, i.e. a top-level form or right under a (hide ...):
;;   sequel is the already-elaborated following AST.
;; else:
;;   sequel is #f.
(define (elaborate context e sequel)

  (define (elab subexpression)
    (elaborate context subexpression #f))

  (define (elaborate-make opt-name clauses)
    (let ((new-context (extend-context context opt-name)))
      (define (elaborate-clause clause)
        (mcase clause
          ((cue params . body)
           (check-clause-syntax clause cue params body)
           `(,cue ,params ,(elaborate-hide new-context body)))))
      `(%make ,(fq-name<- new-context)
         ,@(map elaborate-clause clauses))))

  (cond ((and (pair? e) (look-up-macro (car e)))
         => (lambda (expand) (elaborate context (expand e) sequel)))
        (else
         (mcase e
           ((: _ symbol?)
            (then e sequel))
           (('quote datum)
            (then e sequel))
           ((: _ self-evaluating?)
            (then `',e sequel))
           (('hide . body)
            (then (elaborate-hide context body) sequel))
           (('let '_ e1)
            (then (elab e1) sequel))
           (('let name e1)
            (then-let name (elab e1) sequel))
           (('make '_ . clauses)
            (then (elaborate-make #f clauses) sequel))
           (('make (: name symbol?) . clauses)
            (then-let name (elaborate-make name clauses) sequel))
           (('make . clauses)
            (then (elaborate-make #f clauses) sequel))
           (('include filename)
            (elaborate context `(begin . ,(snarf filename)) sequel))
           (('begin e1 . es)
            (elaborate context e1
                       (if (null? es)
                           sequel
                           (elaborate context `(begin . ,es) sequel))))

           (((: cue cue?) e1 . es)
            (then `(,cue . ,(map elab (cons e1 es)))
                  sequel))
           ((_ . _)
            (then `(.run . ,(map elab e))
                  sequel))))))

(define (elaborate-hide context commands)
  (assert (not (null? commands))
          "Syntax error -- hide" commands)
  (let* ((ast (foldr (lambda (command sequel)
                      (elaborate context command sequel))
                    '(%last) ;; (always removed by unlasting, below)
                    commands))
         (vars (let collecting ((vars '()) (a ast))
                 (mcase a
                   (('%let '_ a1 a2)
                    (collecting vars a2))
                   (('%let v a1 a2)
                    (collecting (cons v vars) a2))
                   (('%last)
                    (reverse vars)))))
         (body (let unlasting ((a ast))
                 (mcase a
                   (('%let '_ a1 ('%last))
                    a1)
                   (('%let v a1 ('%last))
                    `(%let ,v ,a1 ,v))
                   (('%let v a1 a2)
                    `(%let ,v ,a1 ,(unlasting a2)))))))
    (if (null? vars)
        body
        `(%hide ,vars ,body))))

(define (then ast sequel)
  (if sequel
      `(%let _ ,ast ,sequel)
      ast))

(define (then-let name ast sequel)
  (if sequel
      `(%let ,name ,ast ,sequel)
      `(%hide (,name)
         (%let ,name ,ast ,name))))

(define (self-evaluating? x)
  (or (boolean? x)
      (number? x)
      (char? x)
      (string? x)))

(define (check-clause-syntax clause cue params body)
  (assert (not (null? body)) "Empty body" clause)
  (cond ((eq? cue 'else)
         (assert (param-list? params) "Bad param list" clause))
        (else
         (assert (cue? cue) "Not a method decl" clause)
         (assert (or (symbol? params) (param-list? params))
                 "Bad params syntax" clause))))

(define (param-list? x)
  (and (list? x) (all symbol? x)))  ;XXX and distinct

(define (look-up-macro key)
  (mcase key
    ('define (mlambda
              ((_ ((: v symbol?) . params) . body)
               `(make ,v (.run ,params . ,body)))
              ((_ (call-form . params) . body)
               `(define ,call-form (given ,params . ,body)))))
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
     (qq-cons e (expand-quasiquote qcar)
                (expand-quasiquote qcdr)))
    (else `',e)))

(define (qq-cons pair qq-car qq-cdr)
  (mcase (list qq-car qq-cdr)
    ((('quote a) ('quote d))
     `',(reuse-cons pair a d))
    (_ `(',cons ,qq-car ,qq-cdr))))

(define (reuse-cons pair a d)
  (if (and (eqv? (car pair) a)
           (eqv? (cdr pair) d))
      pair
      (cons a d)))
