;; (Roughly) undo parse-exp and parse-pat.
;; Really we should track source-position info instead, and report that.
;; This is just to make debugging less painful till then.

(to (unparse-exp exp)
  (match exp
    ({constant c}
     (if (self-evaluating? c) c `',c))
    ({variable v}
     v)
    ({make _ _ _ _}
     (unparse-make exp))
    ({do _ _}
     (unparse-do exp))
    ({let p e}
     `(let ,(unparse-pat p) ,(unparse-exp e)))
    ({call operator {list operands}}
     `(,(unparse-exp operator) ,@(each unparse-exp operands)))
    ({call operator {term (: cue cue?) operands}}
     `(,(unparse-exp operator) cue ,@(each unparse-exp operands)))
    ({call e1 e2}
     `(call ,(unparse-exp e1) ,(unparse-exp e2)))
    ({term tag es}
     (term<- tag (each unparse-exp es)))
    ({list es}
     `(list<- ,@(each unparse-exp es))) ;XXX unhygienic
    (_
     (error "Unknown expression type" exp))))

(to (unparse-do {do e1 e2})
  (let es
    (begin unparsing ((tail e2))
      (match tail
        ({do e3 e4} (cons e3 (unparsing e4)))
        (_ `(,tail)))))
  `(do ,(each unparse-exp (cons e1 es))))

(to (unparse-make {make name {constant #no} extras-term clauses})
  (match extras-term
    ({constant #no}
     `(make ,name
        ,@(each unparse-clause clauses)))
    ({extending trait-e}
     `(make ,name {extending ,(unparse trait-e)}
        ,@(each unparse-clause clauses)))))

(to (unparse-clause (p p-vars e-vars e))
  `(,(unparse-pat p) ,(unparse-exp e)))

(to (self-evaluating? x)
  (or (claim? x)
      (number? x)
      (char? x)
      (string? x)))

(to (unparse-pat pat)
  (match pat
    ({any-pat}
     '_)
    ({variable-pat v}
     v)
    ({constant-pat c}
     (if (self-evaluating? c) c `',c))
    ({view-pat e p}
     `(<view-pat> ,(unparse-exp e) ,(unparse-pat p))) ;XXX need a genuine view-pat syntax
    ({and-pat p1 p2}
     `(<and-pat> ,(unparse-pat p1) ,(unparse-pat p2))) ;XXX need a genuine and-pat syntax
    ({term-pat tag ps}
     (term<- tag (each unparse-pat ps)))
    (_
     (error "Unknown pattern type" pat))))

(export unparse-exp unparse-pat unparse-clause)
