(import (use "lib/regex-match") regex-match regex-parse)

(to (test regex inputs)
  (let r (regex-parse regex))
  (format "~d\n~d\n" regex ("-" .repeat regex.count))
  (for each! ((str inputs))
    (let m (regex-match r str))
    (format "~d ~w\n" (if m "*" " ") str))
  (newline))

(print (regex-parse "."))

(test "." '("" "a" "ab"))
(test "X" '("hey" "X"))
(test "XY" '("hey" "X" "XY" "XYZ"))
(test "X|Y" '("hey" "X" "Y" "Z"))
(test "X*" '("" "X" "XX" "hey"))
(test "a(b|c)*d" '("ad" "ab" "abcd" "B"))
(test "a.*d" '("ad" "ab" "abcd" "B"))
(test "a[bc]*d" '("ad" "ab" "abcd" "B"))

; TODO more tests
