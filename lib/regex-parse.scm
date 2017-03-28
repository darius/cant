;; Parse regular expressions

(import (use "lib/parson-core") feed push parse)
(import (use "lib/parson-squared") grammar<-)
(import (use "lib/regex-match")
  regex-match
  empty literal either then star
  plus maybe one-of anyone)

(to (parse-regex string)
  ((parse regex-parser string) .result))

;; TODO: something like Python's raw string literals. \\\\ is awful.
(let regex-grammar "
regex    :  exp :end.
exp      :  term ('|' exp     :either)*
         |                    :empty.
term     :  factor (term      :then)*.
factor   :  primary (  '*'    :star
                     | '+'    :plus
                     | '?'    :maybe
                    )?.
primary  :  '(' exp ')'
         |  '[' char* ']'     :join :oneof
         |  '.'               :dot
         |  '\\\\' :anyone      :literal
         |  !( '.' | '(' | ')'
             | '*' | '+' | '?'
             | '|' | '[' | ']')
            :anyone           :literal.
char     :  '\\\\' :anyone
         |  !']' :anyone.
")

(let rp ((grammar<- regex-grammar)
         ;; TODO this is a pain:
         (map<- `(("empty"   ,(push empty))
                  ("literal" ,(feed (given (str) (literal str.first))))
                  ("star"    ,(feed star))
                  ("then"    ,(feed then))
                  ("either"  ,(feed either))
                  ("plus"    ,(feed plus))
                  ("maybe"   ,(feed maybe))
                  ("oneof"   ,(feed one-of))
                  ("dot"     ,(push anyone))
                  ))))
(let regex-parser (rp "regex"))

(export parse-regex)
