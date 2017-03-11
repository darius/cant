(import (use "lib/regex-match") regex-match)
(import (use "lib/regex-parse") parse-regex)

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
(print (regex-match match-X* "hey"))
(newline)

(let re (parse-regex "a(b|c)*d"))
(print (regex-match re "ad"))
(print (regex-match re "ab"))
(print (regex-match re "abcd"))
(print (regex-match re "B"))
(newline)

; TODO more tests
