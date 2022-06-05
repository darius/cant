;; Parse s-expression-syntax Cant expressions and patterns. This
;; desugars them and converts to AST structures. Desugaring includes
;; both expanding built-in macros and desugaring special forms to
;; their core forms. (E.g. `make` is a special form, not a macro, but
;; instances of `make` in Cant source code use more sugar than the
;; `e-make` AST structure does.)

;; We're currently a bit cavalier about hygiene.

#!chezscheme
(library (player parse)
(export parse-e parse-p
        look-up-macro look-up-pat-macro 
        parse-exp parse-pat
        self-evaluating?
        )
(import (chezscheme) (player util) (player macros) (player ast))

(define (parse-exp e . opt-context)
  (parse-e e (optional-context 'parse-exp opt-context)))

(define (parse-pat p . opt-context)
  (parse-p p (optional-context 'parse-pat opt-context)))

(define (optional-context caller opt-context)
  (cond ((null? opt-context) '())
        ((null? (cdr opt-context)) (car opt-context))
        (else (error caller "Too many arguments" opt-context))))

(define (nest-context name ctx)
  (cons name ctx))

(define (parse-e e ctx)
  (cond
    ((and (pair? e) (look-up-macro (car e)))
     => (lambda (expand)
          (let ((new (expand e)))
;            (write `(old ,e)) out.newline
;            (write `(new ,new)) out.newline
;            out.newline
            (parse-e new ctx))))
    (else
     (mcase e
       ((: __ symbol?)
        (when (eq? e '~)
          (error 'parse-exp "Forbidden variable name" e))
        (pack<- e-variable #f #f e))
       (('quote datum)
        (pack<- e-constant datum))
       ((: __ self-evaluating?)
        (pack<- e-constant e))
       ((: __ vector?)
        (parse-e `(array<- ,@(vector->list e)) ctx)) ;XXX hygiene
       ((: __ term?)
        (parse-term-e (term-tag e) (term-parts e) ctx))
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
;;       (('do e1 ('XXX-halp-spot start end) . es)
;;        (pack<- e-do (parse-e `(__halp-log ,start ,end ,e1) ctx)  ;XXX hygiene
;;                (parse-e `(do ,@es) ctx)))
       (('do e1 . es)
        (pack<- e-do (parse-e e1 ctx) (parse-e `(do ,@es) ctx)))
       (('do)
        (pack<- e-constant (void)))          ;I guess
       (('call e1 e2)
        (pack<- e-call (parse-e e1 ctx) (parse-e e2 ctx)))
       (('~ . operands)
        (parse-message-e operands ctx))
       ((addressee . operands)
        (pack<- e-call
                (parse-e addressee ctx)
                (parse-message-e operands ctx)))
       (__ (error 'parse-exp "Bad syntax" e))
       ))))

(define (parse-message-e es ctx)
  ;; TODO the code generated for when there's a spread operator needs to
  ;;  ensure the result is a list
  (mcase es
    (((: cue cue?) . operands)
     (parse-term-e cue operands ctx))
    (operands
     (parse-term-e '~ operands ctx))))

(define (parse-term-e tag operands ctx)
  (if (has-spread-operator? operands)
      (parse-e `(',make-term ',tag ,(unspread-e operands)) ctx)
      (pack<- e-term tag (parse-es operands ctx))))

(define (has-spread-operator? parts)
  (any (mlambda (('@ __) #t) (__ #f))
       parts))  ;XXX really only need to check the last one

(define (unspread-e xs)
  (mcase xs
    ('() ''())
    ((('@ last)) last)
    ((('@ x) . tl) `(',append ,x ,(unspread-e tl)))
    ((hd . tl) (cons-unspread-e hd (unspread-e tl)))))


(define (cons-unspread-e hd tl)
  (mcase tl
    ((('quote f) . args)
     (if (eq? f list*) ;; A minor optimization: turn ('#<list*> x ('#<list*> y z)) into ('#<list*> x y z)
         `(',list* ,hd ,@args)
         `(',list* ,hd ,tl)))
    (_ `(',list* ,hd ,tl))))

(define (remove-spread-operator xs)
  (let walking ((xs xs))
    (mcase xs
      ((('@ last)) `(,last))
      ((hd . tl) (cons hd (walking tl))))))

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
        (when (eq? p '~)
          (error 'parse "Forbidden variable name" p))
        (pack<- p-variable #f #f p))
       ((: __ self-evaluating?)
        (pack<- p-constant p))
       (('quote datum)
        (pack<- p-constant datum))
       (('-- . ps)
        (parse-and-pat ps ctx))
       (('-> e1 p1)
        (pack<- p-view (parse-e e1 ctx) (parse-p p1 ctx)))
       (('-> e1 e2 . rest)
        (parse-p `(-> ,e1 (-> ,e2 . ,rest)) ctx))
       ;; XXX complain if you see a bare , or ,@. but this will fall out of disallowing defaulty lists.
       (('list<- . ps)
        (parse-list-pat ps ctx))
       ;; TODO shouldn't the cdr-p pattern in (link ... cdr-p) do a list? check at match-time?
       (('link cdr-p)
        (parse-p cdr-p ctx))
       (('link car-p . rest-ps)
        (make-cons-pat (parse-p car-p ctx)
                       (parse-p `(link ,@rest-ps) ctx)))
       (('term<- tag-p arguments-p)
        (make-term-pat (parse-p tag-p ctx) (parse-p arguments-p ctx)))
       (('~ . ps)
        (parse-message-pat ps ctx))
       ((: __ term?)
        (parse-term-pat p ctx))
       ((: __ vector?)
        ;; TODO optimize if no spread op
        ;; N.B. the spread operator (like [a b @cs] still binds cs to
        ;;  a *list*, not an array. I guess that's surprising? I'm not
        ;;  sure it's bad.
        (parse-p `(-> ',maybe-vector->list (list<- ,@(vector->list p)))
                 ctx))
       (('@ __)                      ;XXX make @vars be some disjoint type
        (error 'parse "An @-pattern must be at the end of a list" p))
       (__
        (error 'parse "Unknown pattern type" p))))))

(define (parse-message-pat ps ctx)
  (mcase ps
    (((: cue cue?) . operands)
     (parse-term-pat (make-term cue operands) ctx))
    (ps
     (parse-term-pat (make-term '~ ps) ctx))))

(define (maybe-vector->list x)
  (and (vector? x) (vector->list x)))

(define (make-term-pat tag-pat arguments-pat)
  ;; This should probably be in the kernel, but I'm going to make up
  ;; an expansion for now.
  ;; (term<- tag arguments) -> (-> explode-term (link tag arguments))
  (pack<- p-view
          explode-term-exp
          (make-cons-pat tag-pat arguments-pat)))

(define (parse-term-pat p ctx)
  (let ((tag (term-tag p))
        (parts (term-parts p)))
    (if (has-spread-operator? parts)
        (make-term-pat (pack<- p-constant tag)
                       (parse-list-pat parts ctx))
        (pack<- p-term tag (parse-ps parts ctx)))))

(define (parse-and-pat ps ctx)
  (mcase ps
    (()         (pack<- p-any))
    ((p)        (parse-p p ctx))
    ((p1 . ps1) (pack<- p-and (parse-p p1 ctx) (parse-and-pat ps1 ctx)))))

(define (look-up-pat-macro key)
  (mcase key
    ('?      (mlambda
              ((__ e)
               `(-> ,e #t)) ;TODO check result with yeah? instead of = #yes
              ((__ e p1)
               `(-- (-> ,e #t) ,p1))))
    ('=      (mlambda
              ((__ e)
               (let ((param (gensym)))
                 `(-> (on (,param) (= ,param ,e)) ;XXX hygiene
                      #t)))))
    ('optional (mlambda
                ((__ . ps)
                 `(-> ,(optional-match-exp (length ps))
                      ,(make-term 'ok (reverse ps))))))
    ('quasiquote (mlambda
                  ((__ quoted)
                   (expand-quasiquote-pat quoted))))
    ('unless (mlambda
              ((__ e1 . es)
               `(-> (on (_) (do ,e1 ,@es))
                    #f))))
    ('when   (mlambda
              ((__ e1 . es)
               `(-> (on (_) (not (do ,e1 ,@es))) ;XXX hygiene
                    #f))))
    ('or     (mlambda
              ((__ . ps)
               ;; TODO fancier semantics that can bind variables? Requiring every branch to bind the same?
               ;; TODO for now just complain if a p binds anything?
               `(? (be? ,@ps)))))
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

(define (explode-term thing)
  (and (term? thing)
       (cons (term-tag thing) (term-parts thing))))

(define explode-term-exp
  (pack<- e-constant explode-term))

(define (parse-list-pat ps ctx)
  (if (not (has-spread-operator? ps))
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
              (pack<- e-variable #f #f '__as-link)
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

(define (parse-clause clause ctx)
  (mcase clause
    (('to pat . body)
     (mcase pat
       (((: head list?) . params)
        ;; TODO I doubt this experimental syntax is worth supporting --
        ;;  it's motivated by consistency with the definition syntax (to ((f x) y) ...)
        ;;  but it enables weird code like (make f (to ((list<- x) y) 42))
        ;; Also, if we really want this consistency, it ought to go into make-trait too.
        (parse-clause `(to ,head (make _ (to (~ ,@params) ,@body)))
                      ctx))
       (__
        (let ((p (parse-p pat ctx))
              (e (parse-e `(do ,@body) ctx)))
          (list p (pat-vars-defined p) (exp-vars-defined e) e)))))))

(define (look-up-macro key)
  (mcase key
    ('hide   (mlambda
              ((__ . es)
               `((on () ,@es)))))
    ('make-trait
             (mlambda
              ((__ (: v symbol?) (: self symbol?) . clauses) ;XXX allow other patterns?
               (let ((msg (gensym)))
                 ;; TODO leave out miranda-trait if there's a catchall already
                 `(to (,v ,self ,msg)
                    (may ,msg
                      ,@(map (mlambda (('to . rest) `(be ,@rest)))
                             clauses)
                      (else (miranda-trait ,self ,msg)))))))) ;XXX hygiene, and XXX make it overridable
    ('may    (mlambda
              ((__ subject . clauses)
               `((given ,@clauses) ,subject))))
    ('given  (mlambda                   ;TODO experiment; also, better name
              ((__ . clauses)
               `(make _
                  ,@(map (mlambda
                          (('be pat . body)
                           (when (mcase pat (('@ __) #t) (__ #f))
                             (error 'parse "Bad pattern: bare '@'" pat))
                           `(to (~ ,pat) ,@body))
                          (('else . body) ;TODO check that it's the last clause
                           `(to (~ _) ,@body))
                          (clause
                           (error 'parse "Bad clause: 'be' or 'else' missing" clause)))
                         clauses)))))
    ('be?    (mlambda
              ((__ . ps)
               `(make _
                  ,@(map (lambda (pat) `(to (~ ,pat) #t))
                         ps)
                  (to (~ _) #f)))))
    ('to     (mlambda
              ((__ (head . params) . body)
               (if (symbol? head)
                   `(make ,head (to (~ ,@params) ,@body))
                   `(to ,head (make _ (to (~ ,@params) ,@body)))))))
    ('on     (mlambda
              ((__ dp . body)
               `(to (_ ,@dp) ,@body))))
    (':      (mlambda
              ((__ . body)
               `(to (_) ,@body))))
    ('::     (mlambda
              ((__ e)
               `(on (it) ,e))))
    ('for    (mlambda
              ((__ fn bindings . body)
               (let ((name-for (if (symbol? fn)
                                   (string-append "for_" (symbol->string fn))
                                   "for:_")))
                 (parse-bindings bindings
                   (lambda (ps es)
                     `(,fn (make ,name-for
                             (to (~ ,@ps) ,@body))
                           ,@es)))))))
    ('begin  (mlambda
              ((__ (: proc symbol?) bindings . body)
               (parse-bindings bindings
                 (lambda (ps es)
                   `((hide (make ,proc (to (~ ,@ps) ,@body)))
                     ,@es))))
              ((__ (: bindings list?) . body)
               `(begin loop ,bindings ,@body))))
    ('if     (mlambda
              ((__ test if-so if-not)
               `(may ,test
                  (be #f ,if-not)
                  (else ,if-so)))))
    ('when   (mlambda
              ((__ test . body)
               `(if ,test (do ,@body) ',(void)))))
    ('unless (mlambda
              ((__ test . body)
               `(if ,test ',(void) (do ,@body)))))
    ('hm     (mlambda
              ((__)
               '(oops "Fell off the end of 'hm'")) ;XXX hygiene
              ((__ ('else . es))          `(do ,@es))
              ((__ ('do . es) . clauses)  `(do ,@es (hm ,@clauses)))
              ((__ ('let . es) . clauses) `(do (let ,@es) (hm ,@clauses)))
              ((__ ('and . es) . clauses) `(and ,@es (hm ,@clauses)))
              ((__ ('or . es) . clauses)  `(or ,@es (hm ,@clauses)))
              ((__ ('if e e1) . clauses)  `(if ,e ,e1 (hm ,@clauses)))
              ((__ ('when e . es) . clauses)   `(if ,e (do ,@es) (hm ,@clauses)))
              ((__ ('unless e . es) . clauses) `(if ,e (hm ,@clauses) (do ,@es)))
              ((__ ('may e . bes) . clauses)   `(may ,e ,@bes (else (hm ,@clauses))))))
    ('and    (mlambda
              ((__) #t)
              ((__ e) e)
              ((__ e . es) `(if ,e (and ,@es) #f))))
    ('or     (mlambda
              ((__) #f)
              ((__ e) e)
              ((__ e . es)
               (let ((t (gensym)))
                 `((on (,t) (if ,t ,t (or ,@es)))
                   ,e)))))
    ('import (mlambda
              ((__ m . names)
               (insist (all symbol? names) "bad syntax" names)
               (let ((map-var (gensym)))
                 `(let (~ ,@names)
                    ((on (,map-var) 
                       (~ ,@(map (lambda (name) `(,map-var ',name))
                                 names)))
                     ,m))))))
    ('export (mlambda
              ((__ . names)
               (insist (all symbol? names) "bad syntax" names)
               `(map<-  ;; XXX unhygienic; was `',the-map<- but that
                            ;; requires importing from player.scm
                 ,@(map (lambda (name) `(~ ',name ,name))
                        names)))))
    ('quasiquote (mlambda
                  ((__ q) (expand-quasiquote q))))
    ('surely (mlambda
              ((__ ok? . arguments)
               (let ((failure-args (if (null? arguments)
                                       `(',ok?)
                                       arguments)))
                 `(unless ,ok?
                    (oops "Assertion failed" ;XXX hygiene
                          ,@failure-args))))))
    ;; Crude debugging aid. TODO make a better one
    ('yo     (mlambda
              ((__ e)
               `((on (v) (out .say "yo ~w: ~w\n" ',e v) v) ;XXX hygiene
                 ,e))
              ((__ msg e)
               `((on (u v) (out .say "yo ~d ~w: ~w\n" u ',e v) v) ;XXX hygiene
                 ,msg ,e))))
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
     ;; TODO note that a ,@ pattern at the end of the vector, in this
     ;;  expansion, will bind to a *list*, not an array. Also, I don't
     ;;  even know exactly what to expect of a pattern like `[a b @,c]
     ;;  -- N.B. different from ,@c -- should this be an error, or what?
     `(-> ',maybe-vector->list ,(list 'quasiquote (vector->list qq))))
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
               (result (list '->
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

)
