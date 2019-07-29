;; https://projecteuler.net/problem=3
;; The largest prime factor of 600851475143.

(import (use 'factor) factor)

(to (euler3 n)
  (call max (factor n)))

;(print (euler3 13195))
(print (euler3 131957))
;(print (euler3 600851475143))
