;; Main program

(import (use "eg/dole/editor") dole)

(to (main args)
  (match args.rest
    (()         (dole #no))
    ((filename) (dole filename))))
