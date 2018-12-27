(let input (list<-string                ;ugh
            ('.name (with-input-file read "eg/advent-of-code/18/data/advent05"))))

(to (reacts? c d)
  (and (= c.uppercase d.uppercase)
       (not= c d)))

(to (reduce polymer)
  (for foldr ((c polymer) (r '()))
    (if (and (not r.empty?) (reacts? c r.first))
        r.rest
        `(,c ,@r))))

(let reduced (reduce input))
(format "part 1 ~w\n" reduced.count)

(to (scour atom)
  (for those ((c reduced))
    (not= atom c.uppercase)))

(to (try atom)
  ('.count (reduce (scour atom))))

(format "part 2 ~w\n" (call min (each try (#\A .up-to #\Z))))
