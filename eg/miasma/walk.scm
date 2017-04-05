;; XXX expecting terms is inconsistent with parse.scm's production of lists
(to (walk-code code code-f exp-f)
  (begin walking ((code code))
    (let f (code-f code.tag))
    (match code
      ({bytes signedness arg1 arg2}
       (f (signed? signedness) arg1 (walk-exp arg2 exp-f)))
      ({swap-args arg}
       (f (walking arg)))
      ({mod-r/m arg1 arg2}
       (f (walk-exp arg1 exp-f)
          (walk-exp arg2 exp-f))))))

(to (signed? signedness)
  (match signedness ('i #yes) ('u #no)))

(to (walk-exp exp exp-f)
  (begin walking ((exp exp))
    (match exp
      ((: integer?)       ((exp-f 'literal) exp))
      (`(hereafter)       ((exp-f 'hereafter)))
      (`(arg ,@operands)  (call (exp-f 'arg) operands))
      (`(,op ,arg1 ,arg2) ((exp-f 'op) op (walking arg1) (walking arg2))))))


(to ((unit v) args k)
  (k args v))

(to ((bind m2-proc m1) state k)
  (m1 state (given (state1 v1)
              ((m2-proc v1) state1 k))))

(to ((swapping m) `(,z ,y ,@rest) k)
  (m `(,y ,z ,@rest) k))

(to ((eating m-proc) `(,z ,@rest) k)
  ((m-proc z) rest k))
