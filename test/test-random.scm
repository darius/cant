(import (use 'random) rng<-)

(let rng (rng<- 1234567))

; (format "seed: ~w\n" (random-seed<-))
(for each! ((_ (0 .to< 10)))
  (format "~w " (rng .random-integer 4)))
(newline)

        
