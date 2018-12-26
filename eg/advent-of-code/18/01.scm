(import (use "eg/advent-of-code/advent-utils") cycle scanl/lazy)

(let changes (with-input-file read-all "01.txt"))


(display "Part 1\n")

(print (sum changes))


(display "\nPart 2\n")

(let seen (set<-))
(print
 ('.first
  (for those/lazy ((f (scanl/lazy + 0 (cycle changes))))
    (or (seen f)
        (do (seen .add! f)
            #no)))))
