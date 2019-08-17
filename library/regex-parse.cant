;; Parse regular expressions

(import (use 'parson) feed push parse grammar<-)

;; TODO: something like Python's raw string literals. \\\\ is awful.
(let grammar (grammar<- "
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
         |  '\\\\' :anyone    :literal
         |  !( '.' | '(' | ')'
             | '*' | '+' | '?'
             | '|' | '[' | ']')
            :anyone           :literal.
char     :  '\\\\' :anyone
         |  !']' :anyone.
"))

(to (regex-parser<- builder)
  (let semantics
    (map<- (_ 'empty   (push (builder 'empty)))
           (_ 'literal (feed (builder 'literal)))
           (_ 'star    (feed (builder 'star)))
           (_ 'then    (feed (builder 'then)))
           (_ 'either  (feed (builder 'either)))
           (_ 'plus    (feed (builder 'plus)))
           (_ 'maybe   (feed (builder 'maybe)))
           (_ 'oneof   (feed (builder 'one-of)))
           (_ 'dot     (push (builder 'anyone)))))
  (let parser ((grammar semantics) 'regex))
  (to (parse-regex string)
    ((parse parser string) .result)))

(export regex-parser<-)
