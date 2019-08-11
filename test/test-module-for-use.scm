(display "I am loading left-pad!")
(newline)

(to (left-pad string)
  (if (or string.none? (not= #\space string.first))
      string
      (left-pad string.rest)))

(export left-pad)
