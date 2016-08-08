(load "util.scm")
(load "read.scm")
;(snarf "later/new.scm" squeam-read)  ; or readtest.scm?
(define eg-program (snarf "later/compact-lambda.scm" squeam-read))
(load "parse.scm")
(map parse-exp eg-program)
(map parse-exp (snarf "newboot.scm" squeam-read))

(define (print x)
  (write x)
  (newline))

(load "newestterp.scm")

(print (interpret 42))
(print (interpret ''hello))
;(print (interpret '(make _)))
(print (interpret '((make (xs xs)))))
(print (interpret '((make (xs xs)) 1 2 3)))
(print (interpret '(if #f 1 2)))
(print (interpret '(if #t 1 2)))
(print (interpret '((make ((#f) 'no) (_ 'yes)) #f)))
(print (interpret '((make ((#f) 'no) (_ 'yes)) #t)))
(print (interpret '`(hello ,(if #t 'yes 'no))))
(print (interpret '(2 .+ 3)))
(print (interpret '(let x 55)))
(print (interpret '(do (define (f) 136) (f))))
(print (interpret '(do (define (factorial n)
                         (match n
                           (0 1)
                           (_ (n .* (factorial (n .- 1))))))
                       (factorial 10))))

(run-load "later/compact-lambda.scm")

(define (repl)
  (display "> ")
  (pp (interpret (read)))
  (repl))

(run-load "later/sicp1.scm")
(run-load "later/sicp2.scm")

(run-load "later/lambdacompiler.scm")
(run-load "later/parse.scm")
