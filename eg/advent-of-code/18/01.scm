;; (Use run.scm to run this.)

(let changes (with-input-file read-all data-file))


(display "Part 1\n")

(print (sum changes))


(display "\nPart 2\n")

(let freqs (scanl/lazy + 0 (cycle changes)))
(print (_.first (duplicates<- freqs)))
;;(let seen (set<-))
;;(print (for detect ((freq freqs))
;;         (or (seen freq)
;;             (do (seen .add! freq)
;;                 #no))))
