;; XXX untested
;; TODO: map-exp, map-pat, or map-ast or something

(to (qualify-exp context exp)
  (import (qualifier<- context) qe)
  (qe exp))
  
(to (qualify-pat context pat)
  (import (qualifier<- context) qp)
  (qp exp))

(to (qualifier<- context)

  (to (qe exp)
    (match exp
      ({constant _}   exp)
      ({variable _}   exp)
      ({make _ _ _ _} (qualify-make exp))
      ({do e1 e2}     {do (qe e1) (qe e2)})
      ({let p e}      {let (qp p)
                           (qualify-exp (add-pat-context context p) e)})
      ({call e1 e2}   {call (qe e1) (qe e2)})
      ({term tag es}  {term tag (each qe es)})
      ({list es}      {list (each qe es)})))

  (to (qualify-make {make name stamp-exp extras-term clauses})
    {make (qualify-name context name)
          (qe stamp-exp)
          (match extras-term
            ({constant #no} extras-term)
            ({extending trait-e} {extending (qe trait-e)}))
          (hide
            (import (qualifier<- (add-make-context context name))
              qe qp)
            (for each (((p p-vars e-vars e) clauses))
              `(,(qp p) ,p-vars ,e-vars ,(qe e))))})

  (to (qp pat)
    (match pat
      ({any-pat}         pat)
      ({variable-pat v}  pat)
      ({constant-pat c}  pat)
      ({view-pat e p}    {view-pat (qe e) (qp p)})
      ({and-pat p1 p2}   {and-pat (qp p1) (qp p2)})
      ({term-pat tag ps} {term-pat tag (each qp ps)})))

  (export qe qp))

(to (qualify-name context name)
  (let parts (reverse (add-make-context context name)))
  (symbol<- (":" .join parts)))

(to (add-pat-context context p)
  context)  ;TODO add info from p, in the cases where it helps

(to (add-make-context context name)
  ;; TODO make sure name is a symbol, or convert it
  `(,name.name ,@context))

(export qualify-exp qualify-pat)
