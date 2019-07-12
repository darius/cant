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
    (`(,operator ,e)       {app (exp-parse operator)
                                (exp-parse e)})
    (`(,operator ,e1 ,@es) (exp-parse `((,operator ,e1) ,@es)))))

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

(to (church<-claim claim)
  (match claim
    (#no (lookup prelude-env 'no))
    (#yes (lookup prelude-env 'yes))))

(to (church<-count n)
  (let zero (lookup prelude-env 'zero))
  (let succ (lookup prelude-env 'succ))
  (begin counting ((n n))
    (match n
      (0 zero)
      ((? count?) (apply succ (counting (- n 1)))))))

(to (church<-list xs)
  (let ch-nil (lookup prelude-env 'nil))
  (let ch-link (lookup prelude-env 'link))
  (begin linking ((xs xs))
    (match xs
      ('() ch-nil)
      (`(,h ,@t) (apply (apply ch-link h) (linking t))))))

(let builtins
  (map<- `(
           (church<-claim {primitive ,church<-claim})
           (add1 {primitive ,(given (n) (+ n 1))})
           (church<-count {primitive ,church<-count})
           (squeam-link {primitive ,(given (h)
                                      {primitive (given (t) (link h t))})})
           (church<-list {primitive ,church<-list})
           (display {primitive ,display})
           (error {primitive ,(make lambda-error
                                (`(,_) (error "Error in lambda-calculus program")))})
           )))

(let prelude
  '(do 

     (to (no if-yes if-no) if-no)
     (to (yes if-yes if-no) if-yes)
     (to (claim<-church p) (p #yes #no))

     (to (if pred y n) (pred y n))
     (to (and p q) (if p q no))
     (to (or p q) (if p yes q))
     (to (not1 p) (p no yes)) ;; Right? Wikipedia sez "only correct for normal order". Why?
     (to (not p a b) (p b a))  ;; What they say is right for applicative order.

     (to (zero f x) x)
     (to (succ n f x) (f (n f x)))
     (to (count<-church n) (n add1 0))

     (to (+ m) (m succ))
     (to (* m n) (m (+ n) zero))
     (to (expt m n) (n m))

     (to (nil if-link if-nil) if-nil)
     (to (link h t if-link if-nil) (if-link h (t if-link if-nil)))
     (to (list<-church xs) (xs squeam-link '()))

     (to (chain xs ys) (xs link ys))

     (to (compose f g x) (f (g x)))
     (to (identity x) x)

     (to (fix maker)
       (to (r recur) (maker ([x] (recur recur x))))
       (r r))

     (the-environment)))

(let prelude-env (terp prelude {builtins}))


;; Main

(to (main argv)
  (for each! ((filename argv.rest))
    (format "\n~d:\n" filename)
    (print (run filename))))

(to (run filename)
  (let es (with-input-file read-all filename))
  (terp `(do ,@es)))

(export run)
