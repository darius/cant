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
  (may x
    (be `(,(? symbol? sym) ,operand)
      (may (abbrevs .get sym)
        (be #no    (call-like "(" sym x.rest ")"))
        (be abbrev (group (<> (text abbrev) (nest abbrev.count (doc<-sx operand)))))))
    (be (? list?)  (maybe-call-like "(" x ")"))
    (be (? term?)  (maybe-call-like "{" `(,x.tag ,@x.arguments) "}"))
    (be (? array?) (hug "[" x "]"))
    (be (? box?)   (call-like "#<" 'box `(,x.^) ">"))
    (else          (text ("~w" .format x)))))

(to (hug open elements close)
  (group (<> (text open)
             (nest open.count (docs<- elements))
             (text close))))

(to (call-like open symbol elements close)
  (hug (chain open symbol.name " ") elements close))

(to (maybe-call-like open elements close)
  (may elements
    (be `(,(? symbol? sym) ,_ ,@_)
      (call-like open sym elements.rest close))
    (else
      (hug open elements close))))

(to (docs<- sexprs)
  (<> @(intercalate line (each doc<-sx sexprs))))

(export
  pp doc<-sx)
