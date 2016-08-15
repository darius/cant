(import (use "lib/unify.scm") unify empty-subst reify)

(let s (unify empty-subst '(a b) '(b 2)))
(print s)
(print (s .subst 'a))
(print (reify s 'a))
(print (reify s '()))
(print (reify s '(0 a 1 b)))
