(import (use "eg/sokoban") read-collection)

(let `(,grids ,name) (read-collection (string-source<-
"Microban, by David Skinner (just #2)
 #######
 #     #
 # .o. #
## oio #
#  .o. #
#      #
########
")))

(let g (grids 0))
(format "~d\n\n" name)
(format "~d\n" g.unparse)

(g .push 'down)  (format "~d\n" g.unparse)
(g .push 'down)  (format "~d\n" g.unparse)
(g .push 'right) (format "~d\n" g.unparse)

(print g.won?)
