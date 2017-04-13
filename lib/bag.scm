;; A bag is a multiset, i.e. a map from a key to a count of its occurrences.

(to (bag<- @vals)
  (let bag (hash-bag<-))
  (bag .add-all! vals)
  bag)

(to (hash-bag<-)
  (let map (map<-))
  (make bag
    ({.empty?}        map.empty?)
    ({.count}         map.count)
    ({.keys}          map.keys)
    ({.values}        map.values)
    ({.items}         map.items)
    ({.maps? key}     (map .maps? key))
    (`(,key)          (map key)) ;TODO mayyybe instead: (map .get key 0)
    ({.get key default} (map .get key default))
    ({.get key}       (map .get key))
    ({.add! key}      (map .set! key
                           (+ (map .get key 0) 1)))
    ({.add-all! vals} (for each! ((v vals)) (bag .add! v)))
    ({.clear!}        map.clear!)
    ;; XXX fill in rest of bag interface (just the map interface, I guess)
    ({.selfie sink}
     (sink .display "#<bag (")
     (sink .print map.count)
     (sink .display ")>"))
    ))

(export bag<-)
