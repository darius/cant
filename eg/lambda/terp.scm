;; Parsing

(to (seq-parse lexps)
  (match lexps
    (`(,e)                 (exp-parse e))
    (`((let ,p ,e) ,@es)   (exp-parse `(([,p] ,@es) ,e)))
    (`((to ,@_) ,@_)       (def-parse lexps.first lexps.rest))
    (`(,(? array?) ,@_)    (exp-parse lexps)) ;XXX is this a terrible idea?
    ))

(to (def-parse def seq)
  (match def
    (`(to (,(? symbol? name) ,@params) ,@body)
     (let ps (array<-list params))
     (seq-parse `((let ,name (,ps ,@body))
                  ,@seq)))))

(to (exp-parse lexp)
  (match lexp
    ((? symbol?)           {var lexp})
    ((? self-evaluating?)  {const lexp})
    (`',c                  {const c})
    (`(do ,@es)            (seq-parse es))
    (`(,(? array?) ,@_)    (lambda-parse lexp))
    (`(,operator ,@operands)
     (match operands
       (`(,e) {app (exp-parse operator)
                   (exp-parse e)})
       (`(,e1 ,@es) (exp-parse `((,operator ,e1) ,@es)))))))

(to (lambda-parse `(,(? array? params) ,@body))
  ;; TODO this is clumsy without array patterns
  (match params.values
    (`(,(? symbol? v))     {lam v (seq-parse body)})
    (`(,(? symbol? v) ,@vs) {lam v (exp-parse `(,(array<-list vs) ,@body))})))


;; Interpreter

(to (terp e @(optional r))
  (ev (exp-parse e) (or r {builtins})))

(to (ev e r)
  (match e
    ({const c} c)
    ({var v}   (lookup r v))
    ({lam _ _} {closure e r})
    ({app f a} (apply (ev f r) (ev a r)))))

(to (apply fn val)
  (match fn
    ({closure {lam v e} r} (ev e {extend v val r}))
    ({primitive p}         (p val))
    ))


;; Environments, built-ins

(to (lookup r v)
  (match r
    ({extend v1 val r1}
     (if (= v v1)
         val
         (lookup r1 v)))
    ({builtins}
     (builtins v))))

(let builtins
  (map<- `(
           (add1 {primitive ,(given (n) (+ n 1))})
           )))


;; Main

(to (main _)
  (print (terp eg)))

(let eg
  '(do (to (add2 x) (add1 (add1 x)))
       (let no  ([p x y] (p x)))
       (let yes ([p x y] (p y)))
       (to (if pred y n) (pred n y))
       (add2 (add1 5))))
