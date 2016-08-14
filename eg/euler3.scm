;; https://projecteuler.net/problem=3
;; The largest prime factor of 600851475143.

(define (euler3 n)
  (let factors (factor n))
  (factors (- factors.count 1)))

(define (factor n)
  (begin trying ((d 2) (n n))
    (if (= 0 (n .remainder d))
        (cons d (if (= d n) '() (trying d (n .quotient d))))
        (trying (+ d 1) n))))

;(print (euler3 13195))
(print (euler3 131957))
;(print (euler3 600851475143))
