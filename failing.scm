;; An example failing computation, to exercise the debugger.

(load "traceback.scm")

(define (factorial n)
  (if (is? 0 n)
      one
      (.* n (factorial (- n 1)))))

(print (factorial 5))

