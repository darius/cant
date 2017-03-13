(import (use "lib/regex-match") regex-match)
(import (use "lib/regex-parse") parse-regex)

(to (test regex inputs)
  (let r (parse-regex regex))
  (format "~d\n~d\n" regex ("-" .repeat regex.count))
  (for each! ((str inputs))
    (let m (regex-match r str))
    (format "~d ~w\n" (if m "*" " ") str))
  (newline))

(print (parse-regex "."))

(test "." '("" "a" "ab"))

(test "X" '("hey" "X"))

(test "XY" '("hey" "X" "XY" "XYZ"))

(test "X|Y" '("hey" "X" "Y" "Z"))

(test "X*" '("" "X" "XX" "hey"))

(test "a(b|c)*d" '("ad" "ab" "abcd" "B"))

(test "a.*d" '("ad" "ab" "abcd" "B"))

(test "a[bc]*d" '("ad" "ab" "abcd" "B"))

; TODO more tests
