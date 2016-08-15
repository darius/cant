(define (set<- @vals)            ;XXX this name is better saved for frozen sets
  (let s (hash-set<-))
  (s .add-all! vals)
  s)

(define (hash-set<-)
  (let map (map<-)) ;TODO would be nice to avoid storing all the #yes values
  (make hash-set
    ({.keys}          map.keys)
    ({.maps? key}     (map .maps? key))
    ({.diverge}       (call set<- map.keys)) ;TODO tune
    ({.add! key}      (map .set! key #yes))
    ({.add-all! vals} (for each! ((v vals)) (hash-set .add! v)))
    ({.union! other}  (hash-set .add-all! other.keys))
    ({.empty?}        map.empty?)
    ;; XXX fill in rest of set interface (just the map interface, I guess)
    ({.selfie sink}
     (sink .display "#<set")
     (sink .print map.keys)
     (sink .display ">"))
    ))

(define (union set1 set2)      ;XXX name clash with lambdacompiler.scm
  (let result set1.diverge)
  (result .union! set2)
  result)

(hide
 (let a (set<-))
 (print ((union (set<- 1) (set<- 3)) .keys))
)

