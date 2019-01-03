(import (use "eg/advent-of-code/utils") duplicates<- deletions<-)

(let input (with-input-file '.read-lines "eg/advent-of-code/18/data/02.txt"))


(display "Part 1\n")

(to (checksum ids)
  (let bags (each bag<- ids))
  (to ((having n) bag)
    (bag.values .find? n))
  (* (tally (having 2) bags)
     (tally (having 3) bags)))

(print (checksum input))


(display "Part 2\n")

(print (call chain ('.first (duplicates<- (gather deletions<- input)))))
