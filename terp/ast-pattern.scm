;; Script for parsed pattern ASTs

;; It's tricky to load these so that the interpreter can use
;; them... using the interpreter.

(make _
  (`(,me ,message)
   (be message
     ({.selfie sink}
      (sink .display "#<patt ")
      (sink .print me.term)
      (sink .display ">"))
     ({.term}
      (be (__ast-tag me)
        (0 {constant-pat (__ast-part me 1)})
        (1 {any-pat})
        (2 {variable-pat (__ast-part me 1)})
        (3 {term-pat (__ast-part me 1) (__ast-ps me 2)})
        (4 {list-pat (__ast-ps me 1)})
        (5 {and-pat (__ast-p me 1) (__ast-p me 2)})
        (6 {view-pat (__ast-e me 1) (__ast-p me 2)})))
     (_ (miranda-trait me message)))))
