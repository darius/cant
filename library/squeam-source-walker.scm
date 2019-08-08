;; Find the immediate subparts of expressions and patterns, for
;; source-code walking.
;; XXX duplication of terp/parse.scm

(let none '(() ()))

;; Return a pair `(,exprs ,patts) of the immediate subexpressions and
;; subpatterns of expr.
(to (expr-subparts expr)
  (may (macroexpand-outer-expr expr)
    (be (? symbol?)                    none)
    (be (? self-evaluating?)           none)
    (be `(quote ,_)                    none)
    (be (? term? t)                    `(,t.arguments ()))
    (be `(let ,p ,e)                   `((,e) (,p)))
    (be `(make ,(? symbol?) ,@clauses) (make-subparts clauses))
    (be `(make ,(? string?) ,@clauses) (make-subparts clauses))
    (be `(make ,@clauses)              (make-subparts clauses))
    (be `(do ,@es)                     `(,es ()))
    (be `(call ,e1 ,e2)                `((,e1 ,e2) ()))
    (be `(,e1 ,(? cue?) ,@es)          `((,e1 ,@es) ()))
    (be `(,e1 ,@es)                    `((,e1 ,@es) ()))))

(to (make-subparts tail)
  (may tail
    (be `({extending ,e} ,@clauses)
      (let `(,es ,ps) (clauses-subparts clauses))
      `((,e ,@es) ,ps))
    (be clauses
      (clauses-subparts clauses))))

(to (clauses-subparts clauses)
  (may (transpose (each clause-subparts clauses))
    (be '() none)
    (be `(,es-lists ,ps-lists) `(,(chain @es-lists)
                                 ,(chain @ps-lists)))))

(to (clause-subparts `(to ,p ,@es))
  `(,es (,p)))

(to (macroexpand-outer-expr expr)
  (may (maybe-macroexpand-expr expr)
    (be #no
      expr)
    (be {ok expanded}
     ;; Keep expanding the outermost expr until we get core
     ;; syntax. But N.B. we leave subexpressions unexpanded.
     (macroexpand-outer-expr expanded))))

(to (macroexpand-outer-patt patt)
  (may (maybe-macroexpand-patt patt)
    (be #no
      patt)
    (be {ok expanded}
      (macroexpand-outer-patt expanded))))

;; Return a pair `(,exprs ,patts) of the immediate subexpressions and
;; subpatterns of patt.
(to (patt-subparts patt)
  (may (macroexpand-outer-patt patt)
    (be (? symbol?)            none)
    (be (? self-evaluating?)   none)
    (be `(quote ,_)            none)
    (be `(and ,@ps)            `(() ,ps))
    (be `(view ,e ,p)          `((,e) (,p)))
    (be (? term? t)            `(() ,(list-subpatts t.arguments)))
    (be `(@ ,_) (error "An @-pattern must be at the end of a list" patt))
    (be (list<- 'quasiquote q) `(() ,(quasiquote-subpatts q)))
    (be (? list? ps)           `(() ,(list-subpatts ps))))) ;TODO remove

(to (quasiquote-subpatts q)
  (may q
    (be (list<- 'unquote p) `(,p))
    (be (list<- (list<- 'unquote-splicing p)) `(,p))
    (be (list<- 'unquote-splicing p)
      (error "A ,@-pattern must be at the end of a list" q))
    (be (link qcar qcdr) `(',qcar ,@(quasiquote-subpatts qcdr)))
    (be (? term?) (qq-term-subpatts q))
    (else '())))

(to (qq-term-subpatts term)
  (surely (symbol? term.tag))           ;XXX require
  (quasiquote-subpatts term.arguments))

(to (list-subpatts p-list)
  (may p-list
    (be `()             '())
    (be `((@ ,p))       `(,p))
    (be `(,head ,@tail) `(,head ,@(list-subpatts tail)))))

(export expr-subparts patt-subparts macroexpand-outer-expr macroexpand-outer-patt)
