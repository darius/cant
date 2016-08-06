(load "util.scm")
(load "read.scm")
;(snarf "later/new.scm" squeam-read)  ; or readtest.scm?
(define eg-program (snarf "later/compact-lambda.scm" squeam-read))
(load "parse.scm")
(map parse-exp eg-program)
(map parse-exp (snarf "newboot.scm" squeam-read))

(load "newestterp.scm")

(define (print x)
  (write x)
  (newline))

(print (interpret 42))
(print (interpret '(make _)))
(print (interpret '((make (xs xs)))))
(print (interpret '((make (xs xs)) 1 2 3)))


;(boot)
