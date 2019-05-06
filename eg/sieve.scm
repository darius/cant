;; Sieve of Eratosthenes benchmark.

(let SIZE 8190)

(let flags (array<-count (+ SIZE 1) #no))      ;XXX original had just SIZE

(to (main _)
  (format "10 iterations\n")
  (let result
    (for foldl ((_ 0) (iter (1 .to 10)))
      (let count (box<- 0))
      (for each! ((i (0 .to SIZE)))     ;TODO .fill! method
        (flags .set! i #yes))
      (for each! ((i (0 .to SIZE)))
        (when (flags i)
          (let prime (+ i i 3))
          (for each! ((k (range<- (+ i prime) (+ SIZE 1) prime)))
            (flags .set! k #no))
          (count .^= (+ count.^ 1))))
      count.^))
  (format "~w primes\n" result))
