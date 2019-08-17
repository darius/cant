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

;; The Cant interpreter and global environment.

(define (listener . opt-args)
  ;; TODO nicer way to call it to start
  (let ((e `(listener ,@(map (lambda (arg) `',arg) opt-args))))
    (cant-interpret e)))

(run-load "abcs/21-sequels.cant")
(run-load "abcs/30-functions.cant")
(run-load "abcs/40-library.cant")
(run-load "abcs/50-top.cant")
(cant-interpret
 '(do
;    (use "test/smoke-test")
    ;; Let's default to traceback-on-error:
    ;; TODO: also stash the error in the-last-error for below
;    (push-signal-handler ((use 'traceback) 'on-error-traceback))
    (import (use 'flexarray)  flexarray<- flexarray<-list)
    (import (use 'sort)       sort sort-by)
    (import (use 'bag)        bag<-)))

;(unless (cant-interpret '(the-last-error .^))
(listener (cdr (command-line)))
;)
