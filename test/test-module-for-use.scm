(display "I am loading left-pad!")
(newline)

(to (left-pad string)
  (if (or string.empty? (not= #\space string.first))
      string
      (left-pad string.rest)))

(export left-pad)
