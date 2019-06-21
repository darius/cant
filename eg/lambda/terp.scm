;; Parsing

(to (seq-parse lexps)
  (match lexps
    ('((the-environment))  {the-env})   ;TODO not meant as part of the actual language
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
  (ev (exp-parse e) (or r prelude-env)))

(to (ev e r)
  (match e
    ({const c} c)
    ({var v}   (lookup r v))
    ({lam _ _} {closure e r})
    ({app f a} (apply (ev f r) (ev a r)))
    ({the-env} r)))

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

(to (church<-count n)
  (let zero (lookup prelude-env 'zero))
  (let succ (lookup prelude-env 'succ))
  (begin counting ((n n))
    (match n
      (0 zero)
      ((? count?) (apply succ (counting (- n 1)))))))

(let builtins
  (map<- `(
           (add1 {primitive ,(given (n) (+ n 1))})
           (church<-count {primitive ,church<-count})
           )))

(let prelude
  '(do 

     (let no  ([p x y] (p x)))
     (let yes ([p x y] (p y)))
     (to (if pred y n) (pred n y))

     (to (zero f x) x)
     (to (succ n f x) (f (n f x)))
     (to (count<-church n) (n add1 0))

     (to (+ m n f x) (m f (n f x)))
     (to (* m n f x) (m (n f) x))
     (to (expt m n) (n m))
     
     (the-environment)))

(let prelude-env (terp prelude {builtins}))


;; Main

(to (main _)
  (print (terp eg)))

(let eg
  '(do 
     (to (one f x) (f x))
     (let two (+ one one))
     (let three (church<-count 3))
     (count<-church (expt two three))))
