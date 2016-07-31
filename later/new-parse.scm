;; Like elaborate.scm for the new scheme

(define (parse-exp e)
  (case
    ((match e
       ((operator @_) (look-up-macro operator)) ;hey, could this use a view pattern?
       (_ #no))
     => (given (expand) (parse-exp (expand e))))
    (else
     (match e
       ((: _ symbol?)
        {variable e})
       (('quote datum)
        {constant datum})
       ((: _ self-evaluating?)
        {constant e})
       (('let p e1)
        {let (parse-pat p) (parse-exp e1)})
       (('make '_ @clauses)
        (parse-make #no clauses))
       (('make (: name symbol?) @clauses)
        {let name (parse-make name clauses)})
       (('make @clauses)
        (parse-make #no clauses))
       (('do e1)
        (parse-exp e1))
       (('do e1 @es)
        {do (parse-exp e1) (parse-exp `(do ,@es))})
       ((addressee (: cue cue?) @operands)
        {call (parse-exp addressee)
              {term cue (each parse-exp operands)}})
       ((addressee @operands)
        {call (parse-exp addressee)
              (build-list (each parse-exp operands))})
       ((: _ term?)
        {term e.tag (each parse-exp e.parts)})
       ))))

(define (build-list es)
  (match es
    (() {constant '()})
    ((head @tail)
     {term 'cons (list<- head (build-list tail))})))

;; what's the syntax for a macro in pattern context?
(define (parse-pat p)
  (match p
    ('_
     {any-pat})
    ((: symbol?)
     {variable-pat p})
    ((: self-evaluating?)
     {constant-pat p})
    (('quote datum)
     {constant-pat datum})
    ((': e)
     {view-pat (parse-exp e) {constant-pat #yes}})
    ((': p1 e)
     {and-pat (parse-pat p1)
              {view-pat (parse-exp e1) {constant-pat #yes}}})
    ((: list?)
     (parse-list-pat p))
    ((: term?)
     {term-pat e.tag (each parse-pat e.parts)})
    ))

(define (parse-list-pat ps)
  (match ps
    (()
     {view-pat {variable '__as-nil}
               {term-pat 'nil '()}}) ;TODO __as-nil, etc.
    (((: v at-variable?))
     {variable-pat v.name})                     ;TODO v.name, at-variable?
    ((head @tail)
     {view-pat {variable '__as-cons}
               {term-pat 'cons (list<- (parse-pat head)
                                       (parse-list-pat tail))}})))

(define (self-evaluating? x)
  (or (boolean? x)
      (number? x)
      (char? x)
      (string? x)))

(define (look-up-macro key)
  (match key
    ('hide   (make
              ((_ @es)
               `((given () ,@es)))))
    ('include (make             ;temporary
               ((_ (: filename string?))
                `(do ,@(snarf filename)))))
    ('define (make
              ((_ ((: v symbol?) @params) @body)
               `(make ,v (,params ,@body)))
              ((_ (call-form @params) @body)
               `(define ,call-form (given ,params ,@body)))))
    ('given  (make
              ((_ vars @body)
               `(make (,vars ,@body)))))
    ('with   (make
              ((_ bindings @body)
               (let (ps es) (parse-bindings bindings))
               `((given ,ps ,@body) ,@es))))
    ('for    (make
              ((_ fn bindings @body)
               (let (ps es) (parse-bindings bindings))
               `(,fn (given ,ps ,@body) ,@es))))
    ('begin  (make
              ((_ (: proc symbol?) bindings @body)
               (let (ps es) (parse-bindings bindings))
               `((hide (define (,proc ,@ps) ,@body))
                 ,@es))))
    ('if     (make
              ((_ test if-so if-not)
               `((',boolean<- ,test)
                 .choose (given () ,if-so)
                         (given () ,if-not)))))
    ('when   (make
              ((_ test @body)
               `(if ,test (do ,@body) #no))))
    ('unless (make
              ((_ test @body)
               `(if ,test #no (do ,@body)))))
    ('case   (make
              ((_) #no)                 ;TODO: generate an error-raising? I think so.
              ((_ ('else @es)) `(do ,@es))
              ((_ (e) @clauses) `(or ,e (case ,@clauses)))
              ((_ (e1 '=> e2) @clauses)
               (let test-var (gensym))
               `(with ((,test-var ,e1))
                  (if ,test-var
                      (,e2 ,test-var)
                      (case ,@clauses)))))
              ((_ (e @es) @clauses)
               `(if ,e (do ,@es) (case ,@clauses)))))
    ('and    (make
              ((_) #yes)
              ((_ e) e)
              ((_ e @es) `(if ,e (do ,@es) #no))))
    ('or     (make
              ((_) #no)
              ((_ e) e)
              ((_ e @es)
               (let t (gensym))
               `(with ((,t ,e)) (if ,t ,t (do ,@es))))))
    ('quasiquote (make
                  ((_ q) (expand-quasiquote q))))
    (_ #no)))

(define (parse-bindings bindings)
  (for each! (((: symbol?) _) bindings)
    'ok)
  `(,(each '.first bindings) ,(each second bindings)))

(define (second xs) (xs 1))

(define (expand-quasiquote e)
  (match e
    (('unquote e1) e1)
    ((('unquote-splicing e1)) e1)
    ((('unquote-splicing e1) @qcdr)
     `(,e1 .chain ,(expand-quasiquote qcdr)))
    ((qcar @qcdr)
     (qq-cons e (expand-quasiquote qcar)
                (expand-quasiquote qcdr)))
    (else `',e)))

(define (qq-cons pair qq-car qq-cdr)
  (match `(,qq-car ,qq-cdr)
    ((('quote car) ('quote cdr))
     `',(reuse-cons pair car cdr))
    (_ `(',cons ,qq-car ,qq-cdr))))

(define (reuse-cons pair car cdr)
  (if (and (= pair.first car)
           (= pair.rest cdr))
      pair
      (cons car cdr)))
