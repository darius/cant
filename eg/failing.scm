;; An example failing computation, to exercise the debugger.

(define (factorial n)
  (if (= 0 n)
      (error "I don't know 0!" n)
      (* n (factorial (- n 1)))))

(print (factorial 5))
