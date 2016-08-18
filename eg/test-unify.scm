(import (use "lib/unify.scm") variable<- unify empty-subst reify)

(let a (variable<- "a" 0))
(let b (variable<- "b" 0))

(let s (unify empty-subst `(,a ,b) `(,b 2)))
(print s)
(print (s .subst a))
(print (reify s a))
(print (reify s '()))
(print (reify s `(0 ,a 1 ,b)))
