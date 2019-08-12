(import (use "eg/games/sokoban") read-collection)

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

(let afterward
  (for foldl ((g g) (move '(down down right)))
    (hey (g .push move)
         (-> (format "~d\n" it.unparse)))))

(print afterward.won?)
