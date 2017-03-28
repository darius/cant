;; Pretty printing s-expressions

(import (use "lib/pretty-layout")
  pretty-print lay-out nil <> text line nest group)

(to (pp sexpr @opt-width)
  (display (call pretty-print `(,(doc<-sx sexpr) ,@opt-width)))
  (newline))

(to (doc<-sx sexpr)
  (match sexpr
    (((: first symbol?) _ @_)
     (group
      (<> (text "(")
          (nest 1 (<> (text first.name) (text " ")
                      (nest (+ first.name.count 1)
                            (call <> (intercalate line (each doc<-sx
                                                             sexpr.rest))))))
          (text ")"))))
    ((: list?)
     (group (<> (text "(")
                (nest 1 (call <> (intercalate line (each doc<-sx sexpr))))
                (text ")"))))
    (_ (text ("~w" .format sexpr)))))

;; Smoke test

(let eg1
  '(to (fact n)
     (if (= n 0) 1 (* n (fact (- n 1))))))

(to (main _)
  (pp eg1 30))

(export
  pp doc<-sx)
