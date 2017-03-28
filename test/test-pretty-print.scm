(import (use "lib/pretty-print") pp)

(let eg1
  '(to (fact n)
     (if (= n 0) 1 (* n (fact (- n 1))))))

(pp eg1 30)
