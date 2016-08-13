;; https://projecteuler.net/problem=4
;; The largest palindrome that's the product of two 3-digit numbers.

(define (euler4 lo bound)
  (maximum
   (for gather ((i (range<- lo bound)))
     (for each ((j (range<- i bound)))
       (let p (* i j))
       (if (palindrome? p) p 0)))))

(define (palindrome? n)
  (let s (string<-number n))      ;TODO use a string port instead
  (<=> s (reverse s)))

(print (euler4 10 100))
;(print (euler4 (range<- 100 1000)))

;; This code'd be adequate on a nice fast Squeam system; but
;; it's absurdly too slow right now. Need a cleverer algorithm.
