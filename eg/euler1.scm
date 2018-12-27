;; https://projecteuler.net/problem=1
;; Find the sum of all the multiples of 3 or 5 below 1000.

(to (euler1 n)
  (sum (for filter ((i (3 .up-to< n)))
         (and (or (= 0 (i .remainder 3))
                  (= 0 (i .remainder 5)))
              i))))

;(print (euler1 10))
(print (euler1 200))
;(print (euler1 1000))
