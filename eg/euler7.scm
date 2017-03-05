;; https://projecteuler.net/problem=7
;; The 10,001st prime number.
;; TODO this lazy-list code is clumsy

(define (euler7 n)
  ((primes) n))

(define (primes)
  (cons/lazy 2 (given () (filter-prime (odd-integers 3)))))

(define (odd-integers n)
  (cons/lazy n (given () (odd-integers (+ n 2)))))

(define (filter-prime ns) 
  (let p ns.first)
  (cons/lazy p (given ()
                 (filter-prime
                  (for filter/lazy ((k ns.rest))
                    (not= 0 (k .remainder p)))))))

;(print (euler7 5))
(print (euler7 100))
;(print (euler7 10000))
;; TODO implement the real sieve
