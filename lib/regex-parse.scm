;; Parse regular expressions

(import (use "lib/parson") feed parse)
(import (use "lib/parson-squared") grammar<-)
(import (use "lib/regex-match")
  regex-match
  empty literal either then star)

(to (parse-regex string)
  ((parse regex-parser string) .result))

(let regex-grammar "
regex   :  exp :end.
exp     :  term ('|' exp :either)*
        |  :empty.
term    :  factor (term :then)*.
factor  :  primary ('*' :star)?.
primary :  '(' exp ')'
        |  !(')' | '|' | '*') :anyone :literal.
")

(let rp ((grammar<- regex-grammar)
         (map<-a-list `(("empty"   ,empty)
                        ("literal" ,(feed (given (str) (literal str.first))))
                        ("star"    ,(feed star))
                        ("then"    ,(feed then))
                        ("either"  ,(feed either))))))
(let regex-parser (rp "regex"))

(export parse-regex)
