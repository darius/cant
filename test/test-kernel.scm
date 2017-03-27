(import (use "eg/kernel") eval global-env)

(let g '(hide
          (to (fact n)
            (match n
              (0 1)
              (_ (* n (fact (- n 1))))))
          (fact 5)))

(print (eval (parse-exp g) global-env))
