;; Script for parsed pattern ASTs

;; It's tricky to load these so that the interpreter can use
;; them... using the interpreter.

(make _
  (to (_ me message)
    (may message
      (be {.selfie sink}
        (sink .display "#<patt ")
        (sink .print me.term)
        (sink .display ">"))
      (be {.term}
        (may (__ast-tag me)
          (be 0 {constant-pat (__ast-part me 1)})
          (be 1 {any-pat})
          (be 2 {variable-pat (__ast-part me 1)})
          (be 3 {term-pat (__ast-part me 1) (__ast-ps me 2)})
          (be 4 {list-pat (__ast-ps me 1)})
          (be 5 {and-pat (__ast-p me 1) (__ast-p me 2)})
          (be 6 {view-pat (__ast-e me 1) (__ast-p me 2)})))
      (else (miranda-trait me message)))))
