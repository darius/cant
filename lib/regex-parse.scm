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
(let regex-parser exp)

(export parse-regex)
