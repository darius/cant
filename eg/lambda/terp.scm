(import (use "eg/lambda/parser") exp-parse)


;; Interpreter

(to (terp e @(optional r))
  (ev (exp-parse e) (or r prelude-env)))

(to (ev e r)
  (match e
    ({const c} c)
    ({var v}     (lookup r v))
    ({lam _ _ _} {closure e r})
    ({app f a _} (apply (ev f r) (ev a r)))
    ({the-env}   r)))

(to (apply fn val)
  (match fn
    ({closure {lam v e _} r} (ev e {extend v val r}))
    ({primitive p}           (p val))
    ))


;; Environments

(to (lookup r v)
  (match r
    ({extend v1 val r1}
     (if (= v v1)
         val
         (lookup r1 v)))
    ({module map r1}
     (let value (map .get v not-an-lc-value))
     (if (= not-an-lc-value value)
         (lookup r1 v)
         value))
    ({empty-env}
     (error "Unbound variable" v))))

(make not-an-lc-value)


;; Prelude, built-ins

(let prelude-sans-builtins
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

     (to (+ m) (m succ))
     (to (* m n) (m (+ n) zero))
     (to (expt m n) (n m))

     (to (nil if-link if-nil) if-nil)
     (to (link h t if-link if-nil) (if-link h (t if-link if-nil)))

     (to (chain xs ys) (xs link ys))

     (to (compose f g x) (f (g x)))
     (to (identity x) x)

     (to (fix maker)
       (to (r recur) (maker ([x] (recur recur x))))
       (r r))

     (the-environment)))

(let prelude-sans-builtins-env (terp prelude-sans-builtins {empty-env}))

(let lc-no  (lookup prelude-sans-builtins-env 'no))
(let lc-yes (lookup prelude-sans-builtins-env 'yes))

(to (church<-claim claim)
  (match claim
    (#no  lc-no)
    (#yes lc-yes)))

(let lc-zero (lookup prelude-sans-builtins-env 'zero))
(let lc-succ (lookup prelude-sans-builtins-env 'succ))

(to (church<-count n)
  (begin counting ((n n))
    (match n
      (0          lc-zero)
      ((? count?) (apply lc-succ (counting (- n 1)))))))

(let lc-nil  (lookup prelude-sans-builtins-env 'nil))
(let lc-link (lookup prelude-sans-builtins-env 'link))

(to (church<-list xs)
  (begin linking ((xs xs))
    (match xs
      ('()       lc-nil)
      (`(,h ,@t) (apply (apply lc-link h) (linking t))))))

(let builtins-env
  {module
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
            ))
   prelude-sans-builtins-env})

(let prelude-using-builtins
  '(do 
     (to (count<-church n) (n add1 0))
     (to (list<-church xs) (xs squeam-link '()))
     (the-environment)))

(let prelude-env (terp prelude-using-builtins builtins-env))


;; Main

(to (main argv)
  (for each! ((filename argv.rest))
    (format "\n~d:\n" filename)
    (print (run filename))))

(to (run filename)
  (let es (with-input-file read-all filename))
  (terp `(do ,@es)))

(export run)
