;; Find the immediate subparts of expressions and patterns, for
;; source-code walking.
;; XXX duplication of terp/parse.scm

(let none '(() ()))

;; Return a pair `(,exprs ,patts) of the immediate subexpressions and
;; subpatterns of expr.
(to (expr-subparts expr)
  (match (macroexpand-outer-expr expr)
    ((: symbol?)                    none)
    ((: self-evaluating?)           none)
    (`(quote ,_)                    none)
    ((: term?)                      `(,expr.arguments ()))
    (`(let ,p ,e)                   `((,e) (,p)))
    (`(make ,(: symbol?) ,@clauses) (clauses-subparts clauses))
    (`(make ,@clauses)              (clauses-subparts clauses))
    (`(do ,@es)                     `(,es ()))
    (`(call ,e1 ,e2)                `((,e1 ,e2) ()))
    (`(,e1 ,(: cue?) ,@es)          `((,e1 ,@es) ()))
    (`(,e1 ,@es)                    `((,e1 ,@es) ()))))

(to (clauses-subparts clauses)
  (let `(,es-lists ,ps-lists) (transpose (each clause-subparts clauses)))
  `(,(call chain es-lists)
    ,(call chain ps-lists)))

(to (clause-subparts `(,p ,@es))
  `(,es (,p)))

(to (macroexpand-outer-expr expr)
  (match (maybe-macroexpand-expr expr)
    (#no expr)
    (expanded
     ;; Keep expanding the outermost expr until we get core
     ;; syntax. But N.B. we leave subexpressions unexpanded.
     (macroexpand-outer-expr expanded))))

;; Return a pair `(,exprs ,patts) of the immediate subexpressions and
;; subpatterns of patt.
(to (patt-subparts patt)
  (match patt
    ((: symbol?)          none)
    ((: self-evaluating?) none)
    (`(quote ,_)          none)
    ((: term?)            `(() ,(list-subpatts patt.arguments)))
    (`(: ,e)              `((,e) ()))
    (`(: ,p ,e)           `((,e) (,p)))
    (`(@ ,_) (error "An @-pattern must be at the end of a list" patt))
    (`(optional ,p)       `(() (,p)))
    (('quasiquote q)      `(() ,(quasiquote-subpatts q))) ;XXX list pattern syntax
    ((: list?)            `(() ,(list-subpatts patt)))))

(to (quasiquote-subpatts q)
  ;;XXX list pattern syntax, all through below
  (match q
    (('unquote p) `(,p))
    ((('unquote-splicing p)) `(,p))
    (('unquote-splicing p)
     (error "A ,@-pattern must be at the end of a list" q))
    ((qcar @qcdr) `(,qcar ,@(quasiquote-subpatts qcdr)))
    ((: term?) (qq-term-subpatts q))
    (_ '())))

(to (qq-term-subpatts term)
  (surely (symbol? term.tag))           ;XXX require
  (quasiquote-subpatts term.arguments))

(to (list-subpatts p-list)
  (match p-list
    (`() '())
    (`((@ ,p)) `(,p))
    (`(,head ,@tail) `(,head ,@(list-subpatts tail)))))

(export expr-subparts patt-subparts)
