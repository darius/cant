;; Pseudo-random number generators
;; TODO: port a better one

;; (random-seed<-) -> seed (From system randomness.)
;; (prng<- seed) -> prng
;; (prng .random-integer n) -> int

(let D 2147483647)

;; Multiplicative congruential
(to (park-miller-prng<- seed)
  (surely (< 0 seed))
  (let state (box<- seed))

  (to (next)
    (let `(,_ ,r) (state.^ .*/mod 16807 D))
    (state .^= r)
    r)

  (make prng
    ({.random-integer n}
     ((next) .modulo n))                ;XXX could be better
    ))

(let prng<- park-miller-prng<-)
(let rng (prng<- 1234567))

(to (random-seed<-)
  (for with-input-file ((source "/dev/urandom"))
    (begin trying ()
      (let seed (read-u32 source))
      (if (< 0 seed D)
          seed
          (trying)))))

;; Read a 4-byte unsigned int, big-endian. TODO should be in a library
(to (read-u32 source)
  (for foldl ((n 0) (_ (range<- 4))) 
    (+ (n .<< 8)
       source.read-char.code)))

(export prng<- rng random-seed<-)
