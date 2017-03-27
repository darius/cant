(import (use "lib/cycle-write") cycle-write)

(to (cp x)
  (cycle-write x)
  (newline))

(cp '(a b c))

(hide
  (let box (box<- 42))
  (let root `(a ,box c))
  (box .^= root)
  (cp root))

