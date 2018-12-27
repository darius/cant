(import (use "eg/advent-of-code/utils") first-duplicate deletions)

(let input (with-input-file '.read-lines "eg/advent-of-code/18/data/02.txt"))


(display "Part 1\n")

(to (checksum ids)
  (let bags (for each ((id ids))
              (call bag<- (list<-string id))))
  (to ((having n) bag)
    (bag.values .find? n))
  (* ('.count (those (having 2) bags))
     ('.count (those (having 3) bags))))

(print (checksum input))


(display "Part 2\n")

(print (call chain (first-duplicate (gather deletions input))))
