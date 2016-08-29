(import (use "lib/parson.scm")
        invert capture either then feed-list feed push seclude delay maybe many at-least-1
        fail empty skip-1 take-1 any-1 skip-any-1 lit-1 lit)

(define (try p text)
  (write text)
  (display " --> ")
  ((p text 0 0 '()) .display)
  (newline))

;(try skip-any-1 "a")
(try any-1 "a")
(try (seclude any-1) "a")
(try (many any-1) "abc")

(let bal (hide
          (let sub-bal (delay (given () bal)))
          (maybe
           (then (lit "(") sub-bal (lit ")") sub-bal))))

(try bal "(abc")
(try bal "()xyz")
(try bal "()()xyz")
(try bal "(()(()))")

(try (many (lit-1 #\space)) "  hey")

(let hug (feed-list identity))

(let sexpr
  (hide
   (let subexpr (delay (given () sexpr)))
   (let comment (then (lit-1 #\;) (many (then (invert (lit-1 #\newline))
                                              skip-any-1))))
   (let __ (many (either (skip-1 '.whitespace?)
                         comment)))
   (seclude
    (then __
         (either (then (lit-1 #\() __ (many subexpr) (lit-1 #\)) __
                       hug)
                 (then (take-1 '.letter?) (many (take-1 '.alphanumeric?)) __
                       (feed (compose symbol<- chain))))))))

(try sexpr "")
(try sexpr "yo")
(try sexpr "(lisp)")
(try sexpr "(lisp (the  GREATEST  ) hurrah)")
(try sexpr "(oops (unbalanced parens -- before unknown chars))")
(try sexpr "(ok ; I am comment-goat.
hi)")
