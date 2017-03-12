(import (use "lib/regex-match") regex-match)
(import (use "lib/regex-parse") parse-regex)

(to (test regex inputs)
  (let r (parse-regex regex))
  (for each! ((str inputs))
    (print (regex-match r str)))
  (newline))

(test "X" '("hey" "X"))

(test "XY" '("hey" "X" "XY" "XYZ"))

(test "X|Y" '("hey" "X" "Y" "Z"))

(test "X*" '("" "X" "XX" "hey"))

(test "a(b|c)*d" '("ad" "ab" "abcd" "B"))

; TODO more tests
