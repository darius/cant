;; https://projecteuler.net/problem=7
;; The 10,001st prime number.
;; TODO this lazy-list code is clumsy

(to (euler7 n)
  ((primes) n))

(to (primes)
  (link/lazy 2 (given () (filter-prime (odd-integers 3)))))

(to (odd-integers n)
  (link/lazy n (given () (odd-integers (+ n 2)))))

(to (filter-prime ns) 
  (let p ns.first)
  (link/lazy p (given ()
                 (filter-prime
                  (for those/lazy ((k ns.rest))
                    (not= 0 (k .remainder p)))))))  ;TODO p .divides? k

;(print (euler7 5))
(print (euler7 100))
;(print (euler7 10000))
;; TODO implement the real sieve
