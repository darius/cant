#! /usr/bin/scheme --program
#!chezscheme
(import (chezscheme)
  (player abcs)
  (player player))

;; The Cant interpreter

(unless (load-abcs)
  (cant-interpret `(start-playing ',(cdr (command-line)))))
