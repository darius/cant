(import (use "lib/random") rng<-)

(let rng (rng<- 1234567))

; (format "seed: ~w\n" (random-seed<-))
(for each! ((_ (range<- 10)))
  (format "~w " (rng .random-integer 4)))
(newline)

        
