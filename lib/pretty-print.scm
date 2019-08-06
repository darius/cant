;; Pretty printing s-expressions

(import (use 'pretty-layout)
  pretty-print <> text line nest group)

(to (pp sexpr @(optional width))
  (display (pretty-print (doc<-sx sexpr) (or width 72)))
  (newline))

(let abbrevs (map<- '((quote      "'")
                      (quasiquote "`")
                      (unquote    ",")
                      (unquote-splicing ",@"))))

(to (doc<-sx x)
  (be x
    (`(,(? symbol? sym) ,operand)
     (be (abbrevs .get sym)
       (#no    (call-like "(" sym x.rest ")"))
       (abbrev (group (<> (text abbrev) (nest abbrev.count (doc<-sx operand)))))))
    ((? list?)  (maybe-call-like "(" x ")"))
    ((? term?)  (maybe-call-like "{" `(,x.tag ,@x.arguments) "}"))
    ((? array?) (hug "[" x "]"))
    ((? box?)   (call-like "#<" 'box `(,x.^) ">"))
    (_          (text ("~w" .format x)))))

(to (hug open elements close)
  (group (<> (text open)
             (nest open.count (docs<- elements))
             (text close))))

(to (call-like open symbol elements close)
  (hug (chain open symbol.name " ") elements close))

(to (maybe-call-like open elements close)
  (be elements
    (`(,(? symbol? sym) ,_ ,@_)
     (call-like open sym elements.rest close))
    (_ (hug open elements close))))

(to (docs<- sexprs)
  (<> @(intercalate line (each doc<-sx sexprs))))

(export
  pp doc<-sx)
