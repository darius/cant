;; A bag is a multiset, i.e. a map from a key to a count of its occurrences.

(make bag<-
  ('()
   (let map (map<-))
   (make bag {extending map-trait} ;; XXX make sure map-trait methods are OK
     ({.empty?}        map.empty?)
     ({.count}         map.count)
     ({.keys}          map.keys)
     ({.values}        map.values)
     ({.items}         map.items)
     ({.maps? key}     (map .maps? key))
     (`(,key)          (map .get key 0)) ; TODO (map key) would be safer... revisit this
     ({.get key default} (map .get key default))
     ({.get key}       (map .get key))
     ({.add! key}      (map .set! key
                            (+ (map .get key 0) 1)))
     ({.add-all! vals} (for each! ((v vals))
                         (bag .add! v)))
     ({.delete! key}   (map .delete! key))
     ({.clear!}        map.clear!)
     ;; XXX fill in rest of bag interface (just the map interface, I guess)
     ({.selfie sink}
      (sink .display "#<bag (")
      (sink .print map.count)
      (sink .display ")>"))
     ))

  (`(,vals)
   (let bag (bag<-))
   (bag .add-all! vals)
   bag))

(export bag<-)
