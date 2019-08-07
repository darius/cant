;; Script for parsed expression ASTs

;; It's tricky to load these so that the interpreter can use
;; them... using the interpreter.

(make _
  (to (_ me message)
    (may message
      (be {.selfie sink}
        (sink .display "#<expr ")
        (sink .print me.term)
        (sink .display ">"))
      (be {.term}
        (may (__ast-tag me)
          (0 {constant (__ast-part me 1)})
          (1 {variable (__ast-part me 1)})
          (2 {term (__ast-part me 1) (__ast-es me 2)})
          (3 {list (__ast-es me 1)})
          (4 {make (__ast-part me 1)
               (__expr '[0 #no])  ;XXX really hacky
               (__ast-e me 2)
               (__ast-clauses me 3)})
          (5 {do (__ast-e me 1) (__ast-e me 2)})
          (6 {let (__ast-p me 1) (__ast-e me 2)})
          (7 {call (__ast-e me 1) (__ast-e me 2)})
          (8 {variable (__ast-part me 1)})
          ))
      (_ (miranda-trait me message)))))
