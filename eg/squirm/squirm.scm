(import (use "eg/squirm/squirm-terp")
  run-file)

(to (main argv)
  (let `(,_ ,filename) argv)
  (run-file filename))
