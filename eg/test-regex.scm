(import (use "lib/regex.scm") parse-regex re-match)

; TODO more tests

(let re (parse-regex "a(b|c)*d"))

(print (re-match re "ad"))
(print (re-match re "ab"))
(print (re-match re "abcd"))
(print (re-match re "B"))
