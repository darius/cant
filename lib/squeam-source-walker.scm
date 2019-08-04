;; Find the immediate subparts of expressions and patterns, for
;; source-code walking.
;; XXX duplication of terp/parse.scm

(let none '(() ()))

;; Return a pair `(,exprs ,patts) of the immediate subexpressions and
;; subpatterns of expr.
(to (expr-subparts expr)
  (be (macroexpand-outer-expr expr)
    ((? symbol?)                    none)
    ((? self-evaluating?)           none)
    (`(quote ,_)                    none)
    ((? term? t)                    `(,t.arguments ()))
    (`(let ,p ,e)                   `((,e) (,p)))
    (`(make ,(? symbol?) ,@clauses) (make-subparts clauses))
    (`(make ,(? string?) ,@clauses) (make-subparts clauses))
    (`(make ,@clauses)              (make-subparts clauses))
    (`(do ,@es)                     `(,es ()))
    (`(call ,e1 ,e2)                `((,e1 ,e2) ()))
    (`(,e1 ,(? cue?) ,@es)          `((,e1 ,@es) ()))
    (`(,e1 ,@es)                    `((,e1 ,@es) ()))))

(to (make-subparts tail)
  (be tail
    (`({extending ,e} ,@clauses)
     (let `(,es ,ps) (clauses-subparts clauses))
     `((,e ,@es) ,ps))
    (clauses
     (clauses-subparts clauses))))

(to (clauses-subparts clauses)
  (be (transpose (each clause-subparts clauses))
    ('() none)
    (`(,es-lists ,ps-lists) `(,(call chain es-lists)
                              ,(call chain ps-lists)))))

(to (clause-subparts `(,p ,@es))
  `(,es (,p)))

(to (macroexpand-outer-expr expr)
  (be (maybe-macroexpand-expr expr)
    (#no expr)
    ({ok expanded}
     ;; Keep expanding the outermost expr until we get core
     ;; syntax. But N.B. we leave subexpressions unexpanded.
     (macroexpand-outer-expr expanded))))

(to (macroexpand-outer-patt patt)
  (be (maybe-macroexpand-patt patt)
    (#no patt)
    ({ok expanded}
     (macroexpand-outer-patt expanded))))

;; Return a pair `(,exprs ,patts) of the immediate subexpressions and
;; subpatterns of patt.
(to (patt-subparts patt)
  (be (macroexpand-outer-patt patt)
    ((? symbol?)          none)
    ((? self-evaluating?) none)
    (`(quote ,_)          none)
    (`(and ,@ps)          `(() ,ps))
    (`(view ,e ,p)        `((,e) (,p)))
    ((? term? t)          `(() ,(list-subpatts t.arguments)))
    (`(@ ,_) (error "An @-pattern must be at the end of a list" patt))
    ((list<- 'quasiquote q) `(() ,(quasiquote-subpatts q)))
    ((? list? ps)         `(() ,(list-subpatts ps))))) ;TODO remove

(to (quasiquote-subpatts q)
  (be q
    ((list<- 'unquote p) `(,p))
    ((list<- (list<- 'unquote-splicing p)) `(,p))
    ((list<- 'unquote-splicing p)
     (error "A ,@-pattern must be at the end of a list" q))
    ((link qcar qcdr) `(',qcar ,@(quasiquote-subpatts qcdr)))
    ((? term?) (qq-term-subpatts q))
    (_ '())))

(to (qq-term-subpatts term)
  (surely (symbol? term.tag))           ;XXX require
  (quasiquote-subpatts term.arguments))

(to (list-subpatts p-list)
  (be p-list
    (`() '())
    (`((@ ,p)) `(,p))
    (`(,head ,@tail) `(,head ,@(list-subpatts tail)))))

(export expr-subparts patt-subparts macroexpand-outer-expr macroexpand-outer-patt)
