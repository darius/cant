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
          (be 0 {constant (__ast-part me 1)})
          (be 1 {variable (__ast-part me 1)})
          (be 2 {term (__ast-part me 1) (__ast-es me 2)})
          (be 3 {list (__ast-es me 1)})
          (be 4 {make (__ast-part me 1)
                  (__expr '[0 #no])  ;XXX really hacky
                  (__ast-e me 2)
                  (__ast-clauses me 3)})
          (be 5 {do (__ast-e me 1) (__ast-e me 2)})
          (be 6 {let (__ast-p me 1) (__ast-e me 2)})
          (be 7 {call (__ast-e me 1) (__ast-e me 2)})
          (be 8 {variable (__ast-part me 1)})
          ))
      (else (miranda-trait me message)))))
