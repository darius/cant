#!chezscheme
(import (chezscheme)
  (player abcs)
  (player player))

;; The Cant interpreter

(if (load-abcs)
    (display "Bailing after error in start-up.\n")
    (cant-interpret `(start-playing ',(cdr (command-line)))))
