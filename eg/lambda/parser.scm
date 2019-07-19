;; Parsing

(to (seq-parse lexps)
  (match lexps
    ('((the-environment))  {the-env})   ;TODO not meant as part of the actual language
    (`(,e)                 (exp-parse e))
    (`((let ,p ,e) ,@es)   (exp-parse `(([,p] ,@es) ,e)))
    (`((to ,@_) ,@_)       (def-parse lexps.first lexps.rest))
;;    (`(,(? array?) ,@_)    (exp-parse lexps)) ;XXX is this a terrible idea?
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
    (`(,operator ,e)       {app (exp-parse operator)
                                (exp-parse e)})
    (`(,operator ,e1 ,@es) (exp-parse `((,operator ,e1) ,@es)))))

(to (lambda-parse `(,(? array? params) ,@body))
  ;; TODO this is clumsy without array patterns
  (match params.values
    (`(,(? symbol? v))     {lam v (seq-parse body)})
    (`(,(? symbol? v) ,@vs) {lam v (exp-parse `(,(array<-list vs) ,@body))})))

(export seq-parse exp-parse)
