(newline)
(let v (flexarray<-))
(print v)
(print (list<- v.snapshot v.none? v.count))
(print (v .push! 42))
(print (v 0))
(print (list<- v.snapshot v.none? v.count))
(print v.pop!)
(print (list<- v.snapshot v.none? v.count))

;; TODO more tests
