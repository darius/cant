;; Parsing

(to (seq-parse lexps)
  (seq-parsing lexps lexps))

(to (exp-parse lexp)
  (exp-parsing lexp lexp))

(to (seq-parsing lexps srcs)
  (be lexps
    ('((the-environment))  {the-env})   ;TODO not meant as part of the actual language
    (`(,e)                 (exp-parsing e srcs.first))
    (`((let ,p ,e) ,@es)   (exp-parsing `(([,p] ,@es) ,e) `(do ,@srcs)))
    (`((to ,@_) ,@_)       (def-parsing lexps.first lexps.rest srcs))
    ))

(to (def-parsing def seq srcs)
  (be def
    (`(to (,(? symbol? name) ,@params) ,@body)
     (let ps (array<-list params))
     (seq-parsing `((let ,name (,ps ,@body))
                    ,@seq)
                  srcs))))

(to (exp-parsing lexp src)
  (be lexp
    ((? symbol?)           {var lexp})
    ((? self-evaluating?)  {const lexp})
    (`',c                  {const c})
    (`(do ,@es)            (seq-parsing es (src-do-parts src)))
    (`(,(? array?) ,@_)    (lambda-parsing lexp src))
    (`(,operator ,e)       {app (exp-parse operator) ;TODO try to pass along (src 0) pre-expansion 
                                (exp-parse e)
                                src})
    (`(,operator ,e1 ,@es) (exp-parse `((,operator ,e1) ,@es))))) ;TODO ditto

(to (src-do-parts src)
  (be src
    (`(do ,@es) es)
    (_ (error "I don't know how to unparse this sequence" src))))

(to (lambda-parsing `(,(? array? params) ,@body) src)
  ;; TODO this is clumsy without array patterns
  (be params.values
    (`(,(? symbol? v))     {lam v (seq-parse body) src})
    (`(,(? symbol? v) ,@vs) {lam v (exp-parse `(,(array<-list vs) ,@body)) src})))

(export seq-parse exp-parse)
