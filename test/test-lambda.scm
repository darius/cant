(import (use "eg/lambda/terp")
  run)

(let test-names '("trivial1.scm"
                  "trivial2.scm"
                  "church.scm"
                  "factorial.scm"
                  ))

(for each! ((test test-names))
  (format "\nTesting ~d\n" test)
  (print (run (chain "eg/lambda/eg/" test))))
