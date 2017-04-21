;; Sets via hashtable

(to (set<- @vals)            ;XXX this name is better saved for frozen sets
  (let s (hash-set<-))
  (s .add-all! vals)
  s)

(to (hash-set<-)
  (let map (map<-)) ;TODO would be nice to avoid storing all the #yes values
  (make hash-set
    ({.empty?}        map.empty?)
    ({.count}         map.count)
    ({.keys}          map.keys)
    ({.maps? key}     (map .maps? key))
    ({.copy}          (call set<- map.keys)) ;TODO tune
    ({.add! key}      (map .set! key #yes))
    ({.add-all! vals} (for each! ((v vals)) (hash-set .add! v)))
    ({.union! other}  (hash-set .add-all! other.keys))
    ({.union other}   (union hash-set other))
    ({.difference other}
     (let result (set<-))
     (for each! ((x map.keys))
       (unless (other .maps? x)
         (result .add! x)))
     result)
    ({.clear!}        map.clear!)
    ({.get key}       (map .maps? key))
    (`(,key)          (map .maps? key))
    ;; XXX fill in rest of set interface (just the map interface, I guess)
    ({.selfie sink}
     (sink .display "#<set")
     (sink .print map.keys)
     (sink .display ">"))
    ))

(to (union set1 set2)
  (let result set1.copy)
  (result .union! set2)
  result)

(to (union-over sets)
  (let result (set<-))
  (for each! ((set sets))
    (result .union! set))
  result)

(export set<- union union-over)
