;; Try some newer pattern-matching features.

(match {whee 1 2 3}
  ({whee a b c} (print c)))

(match {whee 1 2 3}
  ({whee a @x} (print x)))
