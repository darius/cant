;; How fast do you type?

(import (use 'sturm) cbreak-mode render cursor get-key)

(to (main _)
  (cbreak-mode interact))

(to (interact)
  (show 0 "(Start typing...)")
  (let strokes (flexarray<- (get-key)))
  (let start (nano-now))
  (begin typing ()
    (show (/ (- (nano-now) start) 1000000000)
          (string<-list strokes))
    (hm (unless stdin.ready?
          ;; XXX This polling/sleeping approach sucks, but it's
          ;; supported by the underlying Scheme.
          (nanosleep 50000000)  ; 1/20 sec
          (typing))
        (else
          (may (get-key)
            (be 'esc
              'done)
            (be 'backspace
              (unless strokes.empty?
                strokes.pop!)
              (typing))
            (be (? char? key)
              (strokes .push! key)
              (typing))
            (else
              (typing)))))))

(to (show t body)
  (let cps (if (or (= t 0) body.empty?)
               0
               (/ (- body.count 1) t)))
  (let wpm (/ (* cps 60) 5))
  (render `(,("~w seconds  ~w words/minute"
              .format (floor t) (floor wpm))
            "   (Hit Esc to quit.)\n\n"
            ,body ,cursor)))
