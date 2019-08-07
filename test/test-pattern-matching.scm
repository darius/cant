;; Try some newer pattern-matching features.

(may {whee 1 2 3}
  (be {whee a b c} (print c)))

(may {whee 1 2 3}
  (be {whee a @x} (print x)))
