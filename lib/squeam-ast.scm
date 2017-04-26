;; (Roughly) undo parse-exp and parse-pat.
;; Really we should track source-position info instead, and report that.
;; This is just to make debugging less painful till then.

(to (unparse-exp e)
  (match e.term
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
     (match e2.term
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
      (match tail.term
        ({do e3 e4} (cons e3 (unparsing e4)))
        (_ `(,tail)))))
  `(do ,@(each unparse-exp (cons e1 es))))

(to (unparse-make name stamp trait-term clauses)
  (surely (= {constant #no} stamp.term)) ;XXX
  `(make ,name
     ,@(match trait-term.term
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

(to (unparse-pat pat)
  ;; XXX these need updating to the newer pattern syntax
  (match pat.term
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
