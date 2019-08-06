;; Format an infix expression into a string with minimal parens.

(to (format-infix e)
  (fmt e 0))

(to (fmt e p)
  (be e
    ((? number?)
     (string<-number e))
    ((? symbol?)
     e.name)
    (`(,op ,x)
     (be (unaries .get op)
       (#no
        ("~w(~d)" .format op (fmt x 0)))
       (`(,prefix ,postfix)
        (hm (if prefix  (enclose prefix p ("~w~d" .format op (fmt x prefix))))
            (if postfix (enclose prefix p ("~d~w" .format (fmt x prefix) op)))))))
    (`(,op ,x ,y)
     (be (binaries .get op)
       (#no
        ("~w(~d, ~d)" .format op (fmt x 0) (fmt y 0)))
       (`(,left ,right)
        (enclose left p
                 ("~d ~w ~d" .format (fmt x left) op (fmt y right))))))))

(to (enclose inner outer string)
   (if (< inner outer)
       ("(~d)" .format string)
       string))

(let unaries (map<-))
(let binaries (map<-))

(to (def-prefix name p)  (unaries .set! name `(,p #no)))
(to (def-postfix name p) (unaries .set! name `(#no ,p)))

(to (def-infix name p)   (binaries .set! name `(,p ,p.+)))

(def-prefix '- 100)                     ;XXX dunno

(def-infix '+ 10)
(def-infix '- 10)
(def-infix '* 20)
(def-infix '/ 20)
(binaries .set! '^ '(30 30))   ; right-to-left associativity (is this correct?)
