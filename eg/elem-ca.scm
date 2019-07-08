;; Elementary cellular automata

(to (run rule start-bits n-steps)
  (to (step bits)
    (each rule (neighborhoods bits)))
  (show
   (for foldl ((state start-bits) (_ (range<- n-steps)))
     (show state)
     (step state))))

(to (neighborhoods bits)
  (begin neighing ((a 0) (b 0) (bits bits))
    (if bits.empty?
        `((,a ,b 0) (,b 0 0))
        `((,a ,b ,bits.first)
          ,@(neighing b bits.first bits.rest)))))

(to (show bits)
  (let line (string<-list (each "-*" bits)))
  (format "~d\n" (line .center 80)))

(to (rule<-index n)
  (given (`(,a ,b ,c))
    (let i (+ (* a 4) (* b 2) c))
    ((n .>> i) .and 1)))

(run (rule<-index 110) '(1) 30)
