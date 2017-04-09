(to (walk-code code code-f exp-f)
  (begin walking ((code code))
    (match code
      ({bytes signedness arg1 arg2}
       (let signed? (match signedness ('i #yes) ('u #no)))
       (code-f {bytes signed? arg1 (walk-exp arg2 exp-f)}))
      ({swap-args arg}
       (code-f {swap-args (walking arg)}))
      ({mod-r/m arg1 arg2}
       (code-f {mod-r/m (walk-exp arg1 exp-f)
                        (walk-exp arg2 exp-f)})))))

(to (walk-exp exp exp-f)
  (begin walking ((exp exp))
    (match exp
      ((: integer?)         (exp-f {literal exp}))
      ({hereafter}          (exp-f exp))
      ({arg @_}             (exp-f exp))
      ({op rator arg1 arg2} (exp-f
                             {op rator (walking arg1) (walking arg2)})))))

(to ((unit v) args k)
  (k args v))

(to ((bind m2-proc m1) state k)
  (m1 state (given (state1 v1)
              ((m2-proc v1) state1 k))))

(to ((swapping m) `(,z ,y ,@rest) k)
  (m `(,y ,z ,@rest) k))

(to ((eating m-proc) `(,z ,@rest) k)
  ((m-proc z) rest k))

(export walk-code walk-exp unit bind swapping eating)
