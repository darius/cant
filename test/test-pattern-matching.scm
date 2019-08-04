;; Try some newer pattern-matching features.

(be {whee 1 2 3}
  ({whee a b c} (print c)))

(be {whee 1 2 3}
  ({whee a @x} (print x)))
