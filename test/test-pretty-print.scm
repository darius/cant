(import (use "lib/pretty-print") pp)

(let eg1
  '(to (fact n)
     (if (= n 0) 1 (* n (fact (- n 1))))))

(pp eg1 30)

(let eg2
  '(to (walk tree visit)
     (begin walking ((path '()) (tree tree))
       (match tree
         ({leaf symbol}
          (visit symbol (reverse path)))
         ({branch on-0 on-1}
          (walking `(0 ,@path) on-0)
          (walking `(1 ,@path) on-1))))))

(pp eg2 50)

;(pp '#(a b) 10)
