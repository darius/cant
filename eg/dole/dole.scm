;; Main program

(import (use "eg/dole/editor") dole)

(to (main args)
  (dole args.rest.maybe)) ;TODO this is a cheap hack since you don't
                          ;get an informative error on wrong # of args
