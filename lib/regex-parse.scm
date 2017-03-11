;; Parse regular expressions

(import (use "lib/parson") feed parse)
(import (use "lib/parson-squared") grammar<-)
(import (use "lib/regex-match")
  regex-match
  empty lit<- alt<- chain<- star<-)

(to (parse-regex string)
  ((parse regex-parser string) .result))

(let regex-grammar "
regex   :  exp :end.
exp     :  term ('|' exp :alt)*
        |  :empty
        .
term    :  factor (term :chain)*.
factor  :  primary ('*' :star)?.
primary :  '(' exp ')'
        |  !(')' | '|' | '*') :anyone :literal.
")

(to (literal str) (lit<- str.first))

(let g (grammar<- regex-grammar))
(let rp (g (map<-a-list `(("empty"   ,empty)
                          ("literal" ,(feed literal))
                          ("star"    ,(feed star<-))
                          ("chain"   ,(feed chain<-))
                          ("alt"     ,(feed alt<-))))))
(let regex-parser (rp "regex"))

(export parse-regex)
