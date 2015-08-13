;; An example failing computation, to exercise the debugger.

(define (factorial n)
  (if (is? 0 n)
      one
      (.* n (factorial (- n 1)))))

(print (factorial 5))

