(import (use "eg/advent-of-code/utils") cycle scanl/lazy detect duplicates<-)

(let changes (with-input-file read-all "eg/advent-of-code/18/data/01.txt"))


(display "Part 1\n")

(print (sum changes))


(display "\nPart 2\n")

(let freqs (scanl/lazy + 0 (cycle changes)))
(print ('.first (duplicates<- freqs)))
;;(let seen (set<-))
;;(print (for detect ((freq freqs))
;;         (or (seen freq)
;;             (do (seen .add! freq)
;;                 #no))))
