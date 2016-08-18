;; Run a whole lot of examples, none of which should take long.

(print 42)
(print 'hello)
; (print (make _))
(print ((make (xs xs))))
(print ((make (xs xs)) 1 2 3))
(print (if #no 1 2))
(print (if #yes 1 2))
(print ((make ((#no) 'no) (_ 'yes)) #no))
(print ((make ((#no) 'no) (_ 'yes)) #yes))
(print `(hello ,(if #yes 'yes 'no)))
(print (2 .+ 3))
(print (hide (let x 55)))
(print (hide (define (f) 136) (f)))
(print (hide
        (define (factorial n)
          (match n
            (0 1)
            (_ (n .* (factorial (n .- 1))))))
        (factorial 10)))

(define (loud-load filename)
  (newline)
  (display "-------- ")
  (display filename)
  (display " --------")
  (newline)
  (use filename))

(loud-load "lib/memoize.scm")
(loud-load "lib/parson.scm")
(loud-load "lib/regex.scm")
(loud-load "lib/parse.scm")
(loud-load "lib/unify.scm")

(loud-load "eg/test-export-import.scm")
(loud-load "eg/test-quasiquote.scm")
(loud-load "eg/test-strings.scm")
(loud-load "eg/test-continuations.scm")
(loud-load "eg/test-pattern-matching.scm")
(loud-load "eg/test-use.scm")

(loud-load "eg/test-hashmap.scm")
(loud-load "eg/test-format.scm")
(loud-load "eg/test-fillvector.scm")
(loud-load "eg/test-sort.scm")
(loud-load "eg/test-hashset.scm")
(loud-load "eg/test-memoize.scm")
(loud-load "eg/test-regex.scm")
(loud-load "eg/test-parson.scm")
(loud-load "eg/test-parse.scm")
(loud-load "eg/test-unify.scm")
(loud-load "eg/test-complex.scm")

(loud-load "eg/compact-lambda.scm")
(loud-load "eg/sicp1.scm")
(loud-load "eg/sicp2.scm")
(loud-load "eg/lambdacompiler.scm")
(loud-load "eg/intset1.scm")
(loud-load "eg/intset2.scm")
(loud-load "eg/circuitoptimizer.scm")
(loud-load "eg/fizzbuzz.scm")
(loud-load "eg/failing.scm")
(loud-load "eg/lambdaterp.scm")
(loud-load "eg/tictactoe.scm")
