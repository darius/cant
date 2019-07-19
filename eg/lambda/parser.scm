;; Parsing

(to (seq-parse lexps)
  (seq-parsing lexps `(do ,@lexps)))

(to (exp-parse lexp)
  (exp-parsing lexp lexp))

(to (seq-parsing lexps src)
  (match lexps
    ('((the-environment))  {the-env})   ;TODO not meant as part of the actual language
    (`(,e)                 (exp-parsing e (src-seq-first src)))
    (`((let ,p ,e) ,@es)   (exp-parsing `(([,p] ,@es) ,e) `(do ,@src)))
    (`((to ,@_) ,@_)       (def-parsing lexps.first lexps.rest src))
    ))

(to (src-seq-first src)
  (match src
    (`(do ,e ,@_) e)
    (_ (error "I don't know how to unparse this sequence" src))))

(to (def-parsing def seq src)
  (match def
    (`(to (,(? symbol? name) ,@params) ,@body)
     (let ps (array<-list params))
     (seq-parsing `((let ,name (,ps ,@body))
                    ,@seq)
                  src))))

(to (exp-parsing lexp src)
  (match lexp
    ((? symbol?)           {var lexp})
    ((? self-evaluating?)  {const lexp})
    (`',c                  {const c})
    (`(do ,@es)            (seq-parsing es src))
    (`(,(? array?) ,@_)    (lambda-parsing lexp src))
    (`(,operator ,e)       {app (exp-parse operator) ;TODO try to pass along (src 0) pre-expansion 
                                (exp-parse e)
                                src})
    (`(,operator ,e1 ,@es) (exp-parse `((,operator ,e1) ,@es))))) ;TODO ditto

(to (lambda-parsing `(,(? array? params) ,@body) src)
  ;; TODO this is clumsy without array patterns
  (match params.values
    (`(,(? symbol? v))     {lam v (seq-parse body) src})
    (`(,(? symbol? v) ,@vs) {lam v (exp-parse `(,(array<-list vs) ,@body)) src})))

(export seq-parse exp-parse)
