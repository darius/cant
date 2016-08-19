(import (use "lib/unify.scm") unify)

(define (fail s)
  '())

(define (succeed s)
  `(,s))

(define ((== val1 val2) s)
  (match (unify s val1 val2)
    (#no '())
    (s1 `(,s1))))

;; XXX either and both are not quite Kanrenish

(define ((either goal1 goal2) s)
  (interleave (goal1 s) (goal2 s)))

;; TODO: probably ought to be lazy in the head as well as the tail
(define (interleave xs ys)
  (if xs.empty?
      ys
      (cons/lazy xs.first
                 (given () (interleave ys xs.rest)))))

(define ((both goal1 goal2) s)
  (gather/lazy goal2 (goal1 s)))        ;XXX add interleaving here too?

(export fail succeed == either both)
