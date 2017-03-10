(import (use "lib/regex-match") regex-match)
(import (use "lib/regex-parse") parse-regex)

; TODO more tests

(let re (parse-regex "a(b|c)*d"))

(print (regex-match re "ad"))
(print (regex-match re "ab"))
(print (regex-match re "abcd"))
(print (regex-match re "B"))
