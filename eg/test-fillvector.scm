(hide
 (newline)
 (let v (fillvector<-))
 (print v)
 (print (list<- v.snapshot v.empty? v.count))
 (print (v .push! 42))
 (print (v 0))
 (print (list<- v.snapshot v.empty? v.count))
 (print v.pop!)
 (print (list<- v.snapshot v.empty? v.count))
)
;; TODO more tests
