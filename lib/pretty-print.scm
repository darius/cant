;; Pretty printing s-expressions

(import (use "lib/pretty-layout")
  pretty-print <> text line nest group)

(to (pp sexpr @(optional width))
  (display (pretty-print (doc<-sx sexpr) (or width 72)))
  (newline))

(to (doc<-sx x)
  (match x
    (`(quote ,y)
     (group (<> (text "'") (doc<-sx y))))
    ((list<- 'quasiquote y)
     (group (<> (text "`") (doc<-sx y))))
    ((list<- 'unquote y)
     (group (<> (text ",") (doc<-sx y))))
    ((list<- 'unquote-splicing y)
     (group (<> (text ",@") (doc<-sx y))))
    (`(,(? symbol? first) ,_ ,@_)
     (group
      (<> (text "(") (nest 1 (doc<-tagged first.name x.rest)) (text ")"))))
    ((? list?)
     (group (<> (text "(") (nest 1 (docs<- x)) (text ")"))))
    ((? term?)
     (group (<> (text "{")
                (if x.arguments.empty?
                    (doc<-sx x.tag)
                    (nest 1 (doc<-tagged x.tag.name x.arguments)))
                (text "}"))))
    ((? array?)
     (group (<> (text "#(") (nest 2 (docs<- x)) (text ")"))))
    ((? box?)
     (group (<> (text "#<box ") (nest 6 (doc<-sx x.^)) (text ">"))))
    (_ (text ("~w" .format x)))))

(to (doc<-tagged tag-name arguments)
  (<> (text tag-name) (text " ")
      (nest (+ tag-name.count 1) (docs<- arguments))))

(to (docs<- sexprs)
  (call <> (intercalate line (each doc<-sx sexprs))))

(export
  pp doc<-sx)
