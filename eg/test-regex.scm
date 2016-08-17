(import (use "lib/regex.scm") parse-regex regex-match)

; TODO more tests

(let re (parse-regex "a(b|c)*d"))

(print (regex-match re "ad"))
(print (regex-match re "ab"))
(print (regex-match re "abcd"))
(print (regex-match re "B"))
