#!chezscheme
(library (terp parse)
(export pack<- pack-tag 
        e-constant
        e-variable
        e-term
        e-list
        e-make
        e-do
        e-let
        e-call
        e-global
        p-constant
        p-any
        p-variable
        p-term
        p-list
        p-and
        p-view
        parse-e parse-p
        exp-vars-defined pat-vars-defined
        look-up-macro look-up-pat-macro 
        optional-context
        none-exp
        self-evaluating?
        )
(import (chezscheme) (terp util) (terp macros))

;; Parse expressions and patterns to ASTs

(define pack<- vector)

(define (pack-tag vec)
  (vector-ref vec 0))

(define e-constant 0)
(define e-variable 1)
(define e-term     2)
(define e-list     3)
(define e-make     4)
(define e-do       5)
(define e-let      6)
(define e-call     7)
(define e-global   8)  ;; specialized from e-variable

(define p-constant 0)
(define p-any      1)
(define p-variable 2)
(define p-term     3)
(define p-list     4)
(define p-and      5)
(define p-view     6)

(define (optional-context caller opt-context)
  (cond ((null? opt-context) '())
        ((null? (cdr opt-context)) (car opt-context))
        (else (error caller "Too many arguments" opt-context))))

(define (nest-context name ctx)
  (cons name ctx))

(define (parse-e e ctx)
  (cond
    ((and (pair? e) (look-up-macro (car e)))
     => (lambda (expand) (parse-e (expand e) ctx)))
    (else
     (mcase e
       ((: __ symbol?)
        (pack<- e-variable e))
       (('quote datum)
        (pack<- e-constant datum))
       ((: __ self-evaluating?)
        (pack<- e-constant e))
       ((: __ term?)
        (pack<- e-term
                (term-tag e)
                (parse-es (term-parts e) ctx)))
       (('let p e1)
        (pack<- e-let (parse-p p ctx) (parse-e e1 ctx)))
       (('make '_ . clauses)
        (parse-make '_ clauses ctx))
       (('make (: name symbol?) . clauses)
        (pack<- e-let (parse-p name ctx) (parse-make name clauses ctx)))
       (('make (: name string?) . clauses)
        (parse-make name clauses ctx))
       (('make . clauses)
        (parse-make '_ clauses ctx))
       (('do e1)
        (parse-e e1 ctx))
       (('do e1 ('XXX-halp-spot start end) . es)
        (pack<- e-do (parse-e `(__halp-log ,start ,end ,e1) ctx)  ;XXX hygiene
                (parse-e `(do ,@es) ctx)))
       (('do e1 . es)
        (pack<- e-do (parse-e e1 ctx) (parse-e `(do ,@es) ctx)))
       (('do)
        (pack<- e-constant (void)))          ;I guess
       (('call e1 e2)
        (pack<- e-call (parse-e e1 ctx) (parse-e e2 ctx)))
       ((addressee (: cue cue?) . operands)
        (pack<- e-call
                (parse-e addressee ctx)
                (pack<- e-term cue (parse-es operands ctx))))
       ((addressee . operands)
        (pack<- e-call
                (parse-e addressee ctx)
                (pack<- e-list (parse-es operands ctx))))
       (__ (error 'parse-exp "Bad syntax" e))
       ))))

(define (parse-es es ctx)
  (map (lambda (e) (parse-e e ctx)) es))

(define (parse-ps ps ctx)
  (map (lambda (p) (parse-p p ctx)) ps))

;; TODO vector instead of list
(define (parse-clauses clauses ctx)
  (map (lambda (clause) (parse-clause clause ctx)) clauses))

;; what's the syntax for a macro in pattern context?
(define (parse-p p ctx)
  (cond
    ((and (pair? p) (look-up-pat-macro (car p)))
     => (lambda (expand) (parse-p (expand p) ctx)))
    (else
     (mcase p
       ('_
        (pack<- p-any))
       ((: __ symbol?)
        (pack<- p-variable p))
       ((: __ self-evaluating?)
        (pack<- p-constant p))
       (('quote datum)
        (pack<- p-constant datum))
       (('and . ps)
        (parse-and-pat ps ctx))
       (('view e1 p1)
        (pack<- p-view (parse-e e1 ctx) (parse-p p1 ctx)))
       ;; XXX complain if you see a bare , or ,@. but this will fall out of disallowing defaulty lists.
       (('list<- . ps)
        (parse-list-pat ps ctx))
       (('link car-p cdr-p)
        (make-cons-pat (parse-p car-p ctx) (parse-p cdr-p ctx)))
       ((: __ term?)
        (let ((tag (term-tag p))
              (parts (term-parts p)))
          (if (any (mlambda (('@ __) #t) (__ #f)) parts)  ;XXX really only need to check the last one
              (pack<- p-view
                      explode-term-exp
                      (make-cons-pat (pack<- p-constant tag)
                                     (parse-list-pat parts ctx)))
              (pack<- p-term tag (parse-ps parts ctx)))))
       ((: __ vector?)
        (error 'parse "Array patterns not yet implemented" p))
       (('@ __)                      ;XXX make @vars be some disjoint type
        (error 'parse "An @-pattern must be at the end of a list" p))
       ((: __ list?)
        (error 'parse "Old-style list pattern" p)))))) ;TODO better plaint

(define (parse-and-pat ps ctx)
  (mcase ps
    (()         (pack<- p-any))
    ((p)        (parse-p p ctx))
    ((p1 . ps1) (pack<- p-and (parse-p p1 ctx) (parse-and-pat ps1 ctx)))))

(define (look-up-pat-macro key)
  (mcase key
    ('?      (mlambda
              ((__ e)
               `(view ,e #t))
              ((__ e p1)
               `(and (view ,e #t) ,p1))))
    ('optional (mlambda
                ((__ . ps)
                 `(view ,(optional-match-exp (length ps))
                        ,(make-term 'ok (reverse ps))))))
    ('quasiquote (mlambda
                  ((__ quoted)
                   (expand-quasiquote-pat quoted))))
    (__ #f)))

(define (up-to-n-optional n)
  (lambda (arguments)
    (let eating ((n n) (xs arguments) (values '()))
      (cond ((null? xs)
             (let filling ((n n) (values values))
               (if (= n 0)
                   (make-term 'ok values)
                   (filling (- n 1) (cons #f values)))))
            ((= n 0)
             #f)
            (else
             (eating (- n 1) (cdr xs) (cons (car xs) values)))))))

(define (optional-match-exp<- n)
  `',(up-to-n-optional n))

(define optional-matcher-cache
  (list->vector (map optional-match-exp<- '(0 1 2 3))))

(define (optional-match-exp n)
  (if (< n (vector-length optional-matcher-cache))
      (vector-ref optional-matcher-cache n)
      (optional-match-exp<- n)))

(define (expand-definition-pattern dp)
 (mcase dp
   (((: cue cue?) . rest)
    (make-term cue rest))
   ((: __ list?)
    `(list<- ,@dp))
   ((: __ term?)
    dp)
   (__ (error 'parse "Bad definition pattern" dp))))

(define (explode-term thing)
  (and (term? thing)
       (cons (term-tag thing) (term-parts thing))))

(define explode-term-exp
  (pack<- e-constant explode-term))

(define (parse-list-pat ps ctx)
  (if (all (mlambda (('@ __) #f) (__ #t)) ps)
      (pack<- p-list (parse-ps ps ctx)) ; Special-cased just for speed
      (mcase ps
        (()
         (pack<- p-constant '()))
        ((('@ v))
         (pack<- p-and list?-pat (parse-p v ctx)))
        ((head . tail)
         (make-cons-pat (parse-p head ctx)
                        (parse-list-pat tail ctx))))))

(define list?-pat (pack<- p-view
                          (pack<- e-constant list?) ;XXX just check pair?/null?
                          (pack<- p-constant #t)))

(define (make-cons-pat car-pat cdr-pat)
  (if (and (eq? (pack-tag car-pat) p-constant)
           (eq? (pack-tag cdr-pat) p-constant))
      (unpack car-pat (car-value)
        (unpack cdr-pat (cdr-value)
          (pack<- p-constant (cons car-value cdr-value)))) ;TODO avoid re-consing when possible
      (pack<- p-view
              (pack<- e-variable '__as-link)
              (pack<- p-term 'link (list car-pat cdr-pat)))))

(define (self-evaluating? x)
  (or (boolean? x)
      (number? x)
      (char? x)
      (string? x)))

;; XXX what about stamp?
(define (parse-make name stuff ctx)
  (let ((name (cond ((symbol? name) (symbol->string name))
                    ((string? name) name)
                    (else (error 'parse "Illegal name type" name)))))
    (let* ((ctx (nest-context name ctx))
           (qualified-name (string-join ":" ctx)))
      (mcase stuff
        (((: decl term?) . clauses)
         (insist (eq? (term-tag decl) 'extending) "bad syntax" decl)
         (insist (= (length (term-parts decl)) 1) "bad syntax" decl)
         (pack<- e-make qualified-name
                 (parse-e (car (term-parts decl)) ctx)
                 (parse-clauses clauses ctx)))
        (clauses
         (pack<- e-make qualified-name
                 none-exp
                 (parse-clauses clauses ctx)))))))

(define none-exp (pack<- e-constant '#f))

(define (parse-clause clause ctx)
  (mcase clause
    ((pat . body)
     (let ((p (parse-p pat ctx))
           (e (parse-e `(do ,@body) ctx)))
       (list p (pat-vars-defined p) (exp-vars-defined e) e)))))

(define (look-up-macro key)
  (mcase key
    ('hide   (mlambda
              ((__ . es)
               `((given () ,@es)))))
    ('make-trait
             (mlambda
              ((__ (: v symbol?) (: self symbol?) . clauses) ;XXX allow other patterns?
               (let ((msg (gensym)))
                 ;; TODO leave out miranda-trait if there's a catchall already
                 `(to (,v ,self ,msg)
                    (match ,msg
                      ,@clauses
                      (_ (miranda-trait ,self ,msg)))))))) ;XXX hygiene, and XXX make it overridable
    ('be     (mlambda  ; TODO do I like this better than 'match'?
              ((__ subject . clauses)
               `(match ,subject ,@clauses))))
    ('match  (mlambda
              ((__ subject . clauses)
               `((make _ ,@(map (lambda (clause)
                                  (insist (pair? clause)
                                          "invalid clause" clause)
                                  `((list<- ,(car clause)) ,@(cdr clause)))
                                clauses))
                 ,subject))))
    ('to     (mlambda
              ((__ (head . param-spec) . body)
               (let ((pattern (expand-definition-pattern param-spec)))
                 (if (symbol? head)
                     `(make ,head (,pattern ,@body))
                     `(to ,head (make _ (,pattern ,@body))))))))
    ('given  (mlambda
              ((__ dp . body)
               `(to (_ ,@dp) ,@body))))
    ('on     (mlambda  ; TODO do I like this better than 'given'?
              ((__ dp . body)
               `(to (_ ,@dp) ,@body))))
    ('for    (mlambda
              ((__ fn bindings . body)
               (let ((name-for (if (symbol? fn)
                                   (string-append "for_" (symbol->string fn))
                                   "for:_")))
                 (parse-bindings bindings
                   (lambda (ps es)
                     `(,fn (make ,name-for
                             ((list<- ,@ps) ,@body))
                           ,@es)))))))
    ('begin  (mlambda
              ((__ (: proc symbol?) bindings . body)
               (parse-bindings bindings
                 (lambda (ps es)
                   `((hide (make ,proc ((list<- ,@ps) ,@body)))
                     ,@es))))))
    ('if     (mlambda
              ((__ test if-so if-not)
               `(match ,test
                  (#f ,if-not)
                  (_ ,if-so)))))
    ('when   (mlambda
              ((__ test . body)
               `(if ,test (do ,@body) ',(void)))))
    ('unless (mlambda
              ((__ test . body)
               `(if ,test ',(void) (do ,@body)))))
    ('case   (mlambda
              ((__) '(error "No default case")) ;XXX hygiene
              ((__ ('else . es)) `(do ,@es))
;;              ((__ (e) . clauses) `(or ,e (case ,@clauses))) ;TODO: do I ever use this?
              ((__ (e e1 . es) . clauses)
               `(if ,e (do ,e1 ,@es) (case ,@clauses)))))
    ('so     (mlambda                   ;TODO yet another experiment to toss or keep, replacing 'case'
              ((__)
               '(error "No case taken in 'so'")) ;XXX hygiene
              ((__ ('do . es))
               `(do ,@es))
              ((__ ('else . es))
               `(do ,@es))
              ((__ ('if e e1) . clauses)
               `(if ,e ,e1 (so ,@clauses)))
              ((__ ('when e e1 . es) . clauses)
               `(if ,e (do ,e1 ,@es) (so ,@clauses)))
              ((__ ('unless e e1 . es) . clauses)
               `(if ,e (so ,@clauses) (do ,e1 ,@es)))))
    ('and    (mlambda
              ((__) #t)
              ((__ e) e)
              ((__ e . es) `(if ,e (and ,@es) #f))))
    ('or     (mlambda
              ((__) #f)
              ((__ e) e)
              ((__ e . es)
               (let ((t (gensym)))
                 `((given (,t) (if ,t ,t (or ,@es)))
                   ,e)))))
    ('import (mlambda
              ((__ m . names)
               (insist (all symbol? names) "bad syntax" names)
               (let ((map-var (gensym)))
                 `(let (list<- ,@names)
                    (hide (let ,map-var ,m)
                          (',list ,@(map (lambda (name) `(,map-var ',name))
                                         names))))))))
    ('export (mlambda
              ((__ . names)
               (insist (all symbol? names) "bad syntax" names)
               (list 'map<-   ;; XXX unhygienic; was `',the-map<- but that requires importing from terp.scm
                     (list 'quasiquote
                           (map (lambda (name) (list name (list 'unquote name)))
                                names))))))
    ('quasiquote (mlambda
                  ((__ q) (expand-quasiquote q))))
    (__ #f)))

(define (parse-bindings bindings receiver)
  (for-each (lambda (binding)
              (mcase binding
                ((__ __)   ; (used to check here for a variable, but now can be any pattern)
                 'ok)))
            bindings)
  (receiver (map car bindings) (map cadr bindings)))

(define (expand-quasiquote-pat qq)
  (mcase qq
    (('unquote p)
     p)
    ((('unquote-splicing p))
     `(? ',list? ,p))
    ((('unquote-splicing p) . __)
     (error 'parse "A ,@-pattern must be at the end of a list" qq))
    ((qcar . qcdr)
     ;; TODO optimize simple list patterns into p-list forms
     `(link ,(expand-quasiquote-pat qcar)
            ,(expand-quasiquote-pat qcdr)))
    ((: __ term?)
     (expand-qq-term-pat qq))
    ((: __ vector?)
     (error 'parse "Array patterns not yet implemented" qq))
    ;; TODO any other datatypes like vectors with a ,pat inside?
    ;;  We should probably at least try to notice them and complain.
    (__ `',qq)))

(define (expand-qq-term-pat term)
  (let ((tag (term-tag term))
        (parts (term-parts term)))
    ;; XXX duplicate and ugly code. also, TESTME.
    (if (or (not (symbol? tag))
            (any (mlambda (('unquote-splicing __) #t) (__ #f)) parts))
        (let* ((foo (cons tag parts))
               (result (list 'view
                             `',explode-term
                             (list 'quasiquote foo))))
          result)
        (make-term tag (map expand-quasiquote-pat parts)))))

(define (expand-quasiquote e)
  (mcase e
    (('unquote e1) e1)
    ((('unquote-splicing e1)) e1)
    ((('unquote-splicing e1) . qcdr)
     `(',append ,e1 ,(expand-quasiquote qcdr))) ;XXX call .chain method instead?
    ((qcar . qcdr)
     (qq-cons e (expand-quasiquote qcar)
                (expand-quasiquote qcdr)))
    (__ (cond ((term? e) (expand-qq-term e))
              ((vector? e) (expand-qq-vector e))
              (else `',e)))))

(define (expand-qq-vector vec)
  ;;XXX unhygienic, optimizable, doesn't handle ,@vector as opposed to ,@list
  (list '__array<-list (list 'quasiquote (vector->list vec))))

(define (expand-qq-term term)
  (let ((tag (term-tag term))
        (parts (term-parts term)))
    (let ((tag-e (expand-quasiquote tag)))
      (if (or (not (qq-constant? tag-e))
              (any (mlambda (('unquote-splicing __) #t) (__ #f))
                   parts))
          `(',make-term ,tag-e ,(expand-quasiquote parts))
          (let ((part-es (map expand-quasiquote parts)))
            (if (all qq-constant? part-es)
                `',term
                (make-term tag part-es)))))))

(define qq-constant?
  (mlambda (('quote __) #t) (__ #f)))

(define (qq-cons pair qq-car qq-cdr)
  (mcase `(,qq-car ,qq-cdr)
    ((('quote a) ('quote d))
     `',(reuse-cons pair a d))
    (__ `(',cons ,qq-car ,qq-cdr))))

(define (reuse-cons pair a d)
  (if (and (eqv? (car pair) a)
           (eqv? (cdr pair) d))
      pair
      (cons a d)))


;; Variables defined

(define (exp-vars-defined e)
  ((vector-ref methods/exp-vars-defined (pack-tag e))
   e))

(define methods/exp-vars-defined
  (vector
   (lambda (e) '())                         ;e-constant
   (lambda (e) '())                         ;e-variable
   (lambda (e)                              ;e-term
     (unpack e (tag args)
       (flatmap exp-vars-defined args)))
   (lambda (e)                              ;e-list
     (unpack e (args)
       (flatmap exp-vars-defined args)))
   (lambda (e)                              ;e-make
     '())
   (lambda (e)                              ;e-do
     (unpack e (e1 e2)
       (append (exp-vars-defined e1)
               (exp-vars-defined e2))))
   (lambda (e)                              ;e-let
     (unpack e (p1 e1)
       (append (pat-vars-defined p1)
               (exp-vars-defined e1))))
   (lambda (e)                              ;e-call
     (unpack e (e1 e2)
       (append (exp-vars-defined e1)
               (exp-vars-defined e2))))))

(define (pat-vars-defined p)
  ((vector-ref methods/pat-vars-defined (pack-tag p))
   p))

(define methods/pat-vars-defined
  (vector
   (lambda (p) '())                         ;p-constant
   (lambda (p) '())                         ;p-any
   (lambda (p)                              ;p-variable
     (unpack p (var)
       (list var)))
   (lambda (p)                              ;p-term
     (unpack p (tag args)
       (flatmap pat-vars-defined args)))
   (lambda (p)                              ;p-list
     (unpack p (args)
       (flatmap pat-vars-defined args)))
   (lambda (p)                              ;p-and
     (unpack p (p1 p2)
       (append (pat-vars-defined p1)
               (pat-vars-defined p2))))
   (lambda (p)                              ;p-view
     (unpack p (e1 p1)
       (append (exp-vars-defined e1)
               (pat-vars-defined p1))))))

)
