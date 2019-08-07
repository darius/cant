(import (use "eg/lambda/parser") exp-parse)
(import (use "eg/lambda/prelude") build-prelude)


;; Interpreter

(to (terp e @(optional r))
  (ev (exp-parse e) (or r prelude-env)))

(to (ev e r)
  (may e
    (be {const c}   c)
    (be {var v}     (lookup r v))
    (be {lam _ _ _} {closure e r})
    (be {app f a _} (apply (ev f r) (ev a r)))
    (be {the-env}   r)))

(to (apply fn val)
  (may fn
    (be {closure {lam v e _} r} (ev e {extend v val r}))
    (be {primitive p}           (p val))))


;; Environments

(to (lookup r v)
  (may r
    (be {extend v1 val r1}
      (if (= v v1)
          val
          (lookup r1 v)))
    (be {module map}
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
