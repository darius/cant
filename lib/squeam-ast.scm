;; (Roughly) undo parse-exp and parse-pat.
;; Really we should track source-position info instead, and report that.
;; This is just to make debugging less painful till then.

;;XXX duplicate code: ast-expression.scm
(to (wrap-exp e)
  (surely (and (array? e) (< 0 e.count)))
  (match (e 0)
    (0 {constant (e 1)})
    (1 {variable (e 1)})
    (2 {term (e 1) (e 2)})
    (3 {list (e 1)})
    (4 {make (e 1) (e 2) (e 3) (e 4)})
    (5 {do (e 1) (e 2)})
    (6 {let (e 1) (e 2)})
    (7 {call (e 1) (e 2)})
    (_ (error "Ill-formed expression" e))))

(to (unparse-exp e)
  (match (wrap-exp e)
    ({constant c}
     (if (self-evaluating? c) c `',c))
    ({variable v}
     v)
    ({make name stamp trait clauses}
     (unparse-make name stamp trait clauses))
    ({do e1 e2}
     (unparse-do e1 e2))
    ({let p e}
     `(let ,(unparse-pat p) ,(unparse-exp e)))
    ({call e1 e2}
     (match (wrap-exp e2)
       ({list operands}
        `(,(unparse-exp e1) ,@(each unparse-exp operands)))
       ({term (? cue? cue) operands}
        `(,(unparse-exp e1) ,cue ,@(each unparse-exp operands)))
       (_
        `(call ,(unparse-exp e1) ,(unparse-exp e2)))))
    ({term tag es}
     (term<- tag (each unparse-exp es)))
    ({list es}
     `(list<- ,@(each unparse-exp es))))) ;XXX unhygienic

(to (unparse-do e1 e2)
  (let es
    (begin unparsing ((tail e2))
      (match (wrap-exp tail)
        ({do e3 e4} (cons e3 (unparsing e4)))
        (_ `(,tail)))))
  `(do ,@(each unparse-exp (cons e1 es))))

(to (unparse-make name stamp trait-term clauses)
  (surely (= {constant #no} (wrap-exp stamp))) ;XXX
  `(make ,name
     ,@(match (wrap-exp trait-term)
         ({constant #no} '())
         (trait-e `({extending ,(unparse-exp trait-e)})))
     ,@(each unparse-clause clauses)))

(to (unparse-clause `(,p ,p-vars ,e-vars ,e))
  `(,(unparse-pat p) ,(unparse-exp e)))

(to (self-evaluating? x)
  (or (claim? x)
      (number? x)
      (char? x)
      (string? x)))

(to (wrap-pat p)
  (surely (and (array? p) (< 0 p.count)))
  (match (p 0)
    (0 {constant-pat (p 1)})
    (1 {any-pat})
    (2 {variable-pat (p 1)})
    (3 {term-pat (p 1) (p 2)})
    (4 {list-pat (p 1)})
    (5 {and-pat (p 1) (p 2)})
    (6 {view-pat (p 1) (p 2)})
    (_ (error "Ill-formed pattern" p))))

(to (unparse-pat pat)
  ;; XXX these need updating to the newer pattern syntax
  (match (wrap-pat pat)
    ({constant-pat c}
     (if (self-evaluating? c) c `',c))
    ({any-pat}
     '_)
    ({variable-pat v}
     v)
    ({term-pat tag ps}
     (term<- tag (each unparse-pat ps)))
    ({list-pat ps}
     (each unparse-pat ps))
    ({and-pat p1 p2}
     `(<and-pat> ,(unparse-pat p1) ,(unparse-pat p2)))
    ({view-pat e p}
     `(<view-pat> ,(unparse-exp e) ,(unparse-pat p)))))

(export unparse-exp unparse-pat unparse-clause)
