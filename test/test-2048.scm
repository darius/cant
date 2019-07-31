(import (use "eg/games/2048") left down)

(to (exercise board)
  (show-move left board)
  (show-move down board))

(to (show-move dir board)
  (show board)
  (format " ------- ~w:\n" dir)
  (for each! ((moved (dir board)))
    (show moved)
    (newline))
  (newline))

(to (show rows)
  (for each! ((row rows))
    (for each! ((v row))
      (format " ~d" (if (= v 0) "." v)))
    (newline)))

(exercise '((2 4 2 2)
            (2 2 2 2)
            (0 2 0 2)
            (2 0 2 0)))

(exercise '((2 0 0 2)
            (0 0 0 2)
            (0 2 4 4)
            (0 2 2 4)))

(exercise '((2 2 2 4)
            (0 0 0 0)
            (2 2 4 4)
            (0 0 0 0)))
