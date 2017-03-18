;; The embedded logic language. XXX unfinished untested

(import (use "lib/unify") unify)

(to (fail s)
  '())

(to (succeed s)
  `(,s))

(to ((== val1 val2) s)
  (match (unify s val1 val2)
    (#no '())
    (s1 `(,s1))))

;; XXX either and both are not quite Kanrenish

(to ((either goal1 goal2) s)
  (interleave (goal1 s) (goal2 s)))

;; TODO: probably ought to be lazy in the head as well as the tail
(to (interleave xs ys)
  (if xs.empty?
      ys
      (cons/lazy xs.first
                 (given () (interleave ys xs.rest)))))

(to ((both goal1 goal2) s)
  (gather/lazy goal2 (goal1 s)))        ;XXX add interleaving here too?

(export fail succeed == either both)
