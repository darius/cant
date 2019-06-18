;; Return a list of the prime factors of a positive integer.
;; TODO speed

(to (factor n)
  (surely (integer? n))
  (surely (< 0 n))
  (if (= n 1)
      '()
      (begin trying ((d 2) (n n))
        (if (d .divides? n)
            (link d (if (= d n) '() (trying d (n .quotient d))))
            (trying (+ d 1) n)))))

(export factor)
