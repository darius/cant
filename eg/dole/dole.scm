;; Main program

(import (use "editor") dole)

(to (main args)
  (match args.rest
    (()         (dole #no))
    ((filename) (dole filename))))
