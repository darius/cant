;; Parse regular expressions

(import (use "lib/parson") feed parse)
(import (use "lib/parson-squared") grammar<-)
(import (use "lib/regex-match") regex-match
  lit<- alt<- chain<- star<-)

(to (parse-regex string)
  ((parse regex-parser string) .result))

(let regex-grammar "
primary :  '(' exp ')'
        |  !(')' | '|' | '*') :anyone :literal.
factor  :  primary ('*' :star)?.
term    :  factor (term :chain)*.
exp     :  term ('|' exp :alt)*
#        |  :empty
        .
")

(to (literal str) (lit<- str.first))

(let g (grammar<- regex-grammar))
(let rp (g (map<-a-list `(("literal" ,(feed literal))
                          ("star"    ,(feed star<-))
                          ("chain"   ,(feed chain<-))
                          ("alt"     ,(feed alt<-))))))
(let exp (rp "exp"))           ;XXX needs :end too
(let regex-parser exp)         ;XXX

(to (main _)
  (let match-X (parse-regex "X"))
  (print (regex-match match-X "hey"))
  (print (regex-match match-X "X"))
  (newline)

  (let match-XY (parse-regex "XY"))
  (print (regex-match match-XY "hey"))
  (print (regex-match match-XY "X"))
  (print (regex-match match-XY "XY"))
  (print (regex-match match-XY "XYZ"))
  (newline)

  (let match-or (parse-regex "X|Y"))
  (print (regex-match match-or "hey"))
  (print (regex-match match-or "X"))
  (print (regex-match match-or "Y"))
  (print (regex-match match-or "Z"))
  (newline)

  (let match-X* (parse-regex "X*"))
  (print (regex-match match-X* ""))
  (print (regex-match match-X* "X"))
  (print (regex-match match-X* "XX"))
  (print (regex-match match-X* "hey")))

(export parse-regex)
