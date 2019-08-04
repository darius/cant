;; (Use run.scm to run this.)

(let input (with-input-file _.read-lines data-file))


(display "Part 1\n")

(to (checksum ids)
  (let bags (each bag<- ids))
  (to ((having n) bag)
    (bag.values .find? n))
  (* (tally (having 2) bags)
     (tally (having 3) bags)))

(print (checksum input))


(display "Part 2\n")

(print (call chain (_.first (duplicates<- (gather deletions<- input)))))
