(import (use "later/squickcheck")
  a-claim a-nat an-int a-char a-printable-char a-printable-string a-list-of a-tuple a-choice
  weighted-choice
  check should  ;; I dunno what to call it yet
  )

(for check ((a an-int) (b an-int))
  ;; Ints are commutative.
  (should = (+ a b) (+ b a)))

;; A deliberately failing property

(for check ((L (a-list-of a-nat)))
  ;; Lists are palindromic (not!)
  (should = L (reverse L)))

