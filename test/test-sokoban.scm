(import (use "eg/games/sokoban") read-collection)

(let (_ grids name) (read-collection (string-source<-
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
  (for foldl ((g g) (dir '(down down right)))
    (hey (g .move dir)
         (-> (format "~d\n" it.unparse)))))

(print afterward.won?)
