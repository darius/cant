#! /usr/bin/scheme --program
#!chezscheme
(import (chezscheme)
  (player util)
  (player macros)
  (player read)
  (player parse)
  (player env)
  (player elaborate)
  (player primitives)
  (player terp))

;; The Squeam interpreter and global environment.

(define (repl . opt-args)
  (squeam-interpret `(call repl ',opt-args)))

(run-load "abcs/functions.scm")
(run-load "abcs/library.scm")
(squeam-interpret
 '(do
;    (use "test/smoke-test")
    ;; Let's default to traceback-on-error:
    ;; TODO: also stash the error in the-last-error for below
;    (push-signal-handler ((use 'traceback) 'on-error-traceback))
    (import (use 'flexarray)  flexarray<- flexarray<-list)
    (import (use 'sort)       sort sort-by)
    (import (use 'bag)        bag<-)))

;(unless (squeam-interpret '(the-last-error .^))
(repl (cdr (command-line)))
;)
