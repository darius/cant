;; SICP exercise 2.29
;; (I renamed 'total-weight' to just 'weight'.)

;; Algebraic-data version

(let test-mobile
  {mobile {branch 1 10}
          {branch 2 {mobile {branch 3 20}
                            {branch 4 30}}}})

(let test-balanced-mobile
  {mobile {branch 7 10}
          {branch 1 {mobile {branch 3 40}
                            {branch 4 30}}}})

(to (balanced? tree)
  (be tree
    ({mobile left right}
     (and (balanced? left)
          (balanced? right)
          (= (torque left) (torque right))))
    (_
     #yes)))                       ;XXX don't we care about deep balance?

(to (torque {branch length structure})
  (* length (weight structure)))

(to (weight tree)
  (be tree
    ({mobile left right}
     (+ (weight left) (weight right)))
    ((? number?)
     tree)
;; XXX this was missing -- go look up the problem again:
    ({branch _ thing}
     (weight thing))))

(print (balanced? test-mobile))
(print (balanced? test-balanced-mobile))
