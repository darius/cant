(import (use "lib/parson")         parse)
(import (use "lib/parson-squared") grammar<-)

(to (try grammar-text start-symbol subs inputs)
  (let parser ((grammar<- grammar-text) subs))
  (let start (parser start-symbol.name))
  (for each! ((input inputs))
    (print input)
    (let outcome (parse start input))
    outcome.display (newline) (newline)))

(when #no
  (let g (grammar<- text))
  (let subs (map<-))
  (let parser (g subs))
  (let S (parser "split"))
  
  (let outcome (parse S input))
  outcome.display (newline) (newline)
  )

(let junk "
main: r*.
r: .
s: 'hey' r.
")
(try junk 'main (map<-) '())

;; XXX needs :end
(let a-and-b-equal-counts "
S     :  'a' B
      |  'b' A
      |  .

A     :  'a' S
      |  'b' A A.

B     :  'b' S
      |  'a' B B.
")
(let input2 "abaabbbbaa")
(try a-and-b-equal-counts 'S (map<-) `(,input2)) ;XXX wrong result

(let text "
split  :  (p | chunk :join) split | .  # XXX why not a *?
chunk  :  p
       |  :anyone chunk.
p      :  :whitespace.
")
(let input "hello a world  is    nice    ")
(try text 'split (map<-) `(,input))
