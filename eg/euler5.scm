;; https://projecteuler.net/problem=5
;; The smallest positive number that is evenly divisible by all of the
;; numbers from 1 to 20.

(define (euler5 ns)
  ((common-multiples<- ns) .first))

(define (common-multiples<- ns)
  (foldr1 intersect (each multiples<- ns)))

;; Pre: xs and ys are sorted and infinite.
(define (intersect xs ys)
  (match (xs.first .compare ys.first)
    (-1 (intersect xs.rest ys))
    (+1 (intersect xs ys.rest))
    ( 0 (cons/lazy xs.first
                   (given () (intersect xs.rest ys.rest))))))

(define (multiples<- n)
  (begin listing ((k n))
    (cons/lazy k (given () (listing (+ k n))))))

(define (cons/lazy x thunk)
  (make lazy-list
    ({.empty?} #no)
    ({.first}  x)
    ({.rest}   (thunk))
    ;; ... XXX use list-trait? except it'd need a rewrite for laziness
    ))

(print (euler5 (range<- 2 11)))
;(print (euler5 (range<- 2 21)))
;; XXX way too slow again
