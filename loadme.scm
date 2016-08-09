(load "util.scm")
(load "read.scm")
;(snarf "later/new.scm" squeam-read)  ; or readtest.scm?
(define eg-program (snarf "eg/compact-lambda.scm" squeam-read))
(load "parse.scm")
(map parse-exp eg-program)

(define (print x)
  (write x)
  (newline))

(load "terp.scm")

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
(print (interpret '(hide (let x 55))))
(print (interpret '(hide (define (f) 136) (f))))
(print (interpret '(hide
                       (define (factorial n)
                         (match n
                           (0 1)
                           (_ (n .* (factorial (n .- 1))))))
                       (factorial 10))))

(run-load "stdlib.scm")

(define (repl)
  (display "> ")
  (pp (interpret (read)))
  (repl))

(run-load "eg/compact-lambda.scm")

(run-load "eg/sicp1.scm")
(run-load "eg/sicp2.scm")

(run-load "eg/lambdacompiler.scm")
(run-load "eg/parson.scm")
(run-load "eg/parse.scm")
(run-load "eg/intset.scm")
(run-load "eg/circuitoptimizer.scm")
(run-load "eg/fizzbuzz.scm")
