;; https://projecteuler.net/problem=6
;; Find the difference between the sum of the squares of the first one
;; hundred natural numbers and the square of the sum.

(to (euler6 ns)
  (- (square (sum ns))
     (sum-by square ns)))

(to (square n)
  (* n n))

(print (euler6 (1 .to 10)))
;(print (euler6 (1 .to 100)))
