(import (use "lib/parson") parse grammar<-)

(to (try grammar-text start-symbol subs inputs)
  (let parser ((grammar<- grammar-text) subs))
  (let start (parser start-symbol))
  (format "trying ~w: ~w\n" start-symbol start)
  (for each! ((input inputs))
    (print input)
    (let outcome (parse start input))
    outcome.display (newline) (newline)))

(let trivial "
trivial  :  'x'.
")
(try trivial 'trivial (map<-) `("x"))

(let junk "
main: r*.
r: .
s: 'hey' r.
")
(try junk 'main (map<-) '())

(let bal "
bal  :  ('(' bal ')' bal)?.
")
(try bal 'bal (map<-) `("()"))

(let balanced "
bal  :  '(' c* ')'.
c    :  !('(' | ')') :anyone
     |  bal.
")
(try balanced 'c (map<-) `("(())"))

(let a-and-b-equal-counts "
start :  S :end.
S     :  'a' B
      |  'b' A
      |  .

A     :  'a' S
      |  'b' A A.

B     :  'b' S
      |  'a' B B.
")
(try a-and-b-equal-counts 'start (map<-) '("abaabbbbaa"))

(let text "
split  :  (p | chunk :join) split | .  # XXX why not a *?
chunk  :  p
       |  :anyone chunk.
p      :  :whitespace.
")
(let input "hello a world  is    nice    ")
(try text 'split (map<-) `(,input))
