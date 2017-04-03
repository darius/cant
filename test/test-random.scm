(import (use "lib/random") prng<- rng random-seed<-)

; (format "seed: ~w\n" (random-seed<-))
(for each! ((_ (range<- 10)))
  (format "~w " (rng .random-integer 4)))
(newline)

        
