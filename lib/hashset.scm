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
    ({.union other}   (union hash-set other))
    ({.empty?}        map.empty?)
    ({.count}         map.count)
    ;; XXX fill in rest of set interface (just the map interface, I guess)
    ({.selfie sink}
     (sink .display "#<set")
     (sink .print map.keys)
     (sink .display ">"))
    ))

(define (union set1 set2)
  (let result set1.diverge)
  (result .union! set2)
  result)

(define (union-over sets)
  (let result (set<-))
  (for each! ((set sets))
    (result .union! set))
  result)

(export set<- union union-over)
