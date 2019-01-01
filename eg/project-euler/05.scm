;; https://projecteuler.net/problem=5
;; The smallest positive number that is evenly divisible by all of the
;; numbers from 1 to 20.

(to (euler5 ns)
  ((common-multiples<- ns) .first))

(to (common-multiples<- ns)
  (foldr1 intersect (each multiples<- ns)))

;; Pre: xs and ys are sorted and infinite.
(to (intersect xs ys)
  (match (xs.first .compare ys.first)
    (-1 (intersect xs.rest ys))
    (+1 (intersect xs ys.rest))
    ( 0 (cons/lazy xs.first
                   (given () (intersect xs.rest ys.rest))))))

(to (multiples<- n)                     ;TODO infinite range<- with stride
  (begin listing ((k n))
    (cons/lazy k (given () (listing (+ k n))))))

(print (euler5 (2 .to 10)))
;(print (euler5 (2 .to 20)))
;; XXX way too slow again
