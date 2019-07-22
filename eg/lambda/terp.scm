(import (use "eg/lambda/parser") exp-parse)
(import (use "eg/lambda/prelude") build-prelude)


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
    ({primitive p}           (p val))))


;; Environments

(to (lookup r v)
  (match r
    ({extend v1 val r1}
     (if (= v v1)
         val
         (lookup r1 v)))
    ({module map}
     (map v))))

(let prelude-env (build-prelude terp apply))


;; Main

(to (main argv)
  (for each! ((filename argv.rest))
    (format "\n~d:\n" filename)
    (print (run filename))))

(to (run filename)
  (let es (with-input-file read-all filename))
  (terp `(do ,@es)))

(export run terp prelude-env)
