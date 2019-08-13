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
  (player player))

;; The Squeam interpreter and global environment.

(define (listener . opt-args)
  (squeam-interpret `(call listener ',opt-args)))

(run-load "abcs/21-sequels.scm")
(run-load "abcs/30-functions.scm")
(run-load "abcs/40-library.scm")
(run-load "abcs/50-top.scm")
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
(listener (cdr (command-line)))
;)
