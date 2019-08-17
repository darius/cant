;; A bag is a multiset, i.e. a map from a key to a count of its occurrences.

(make bag<-
  (to (_)
    (let map (map<-))
    (make bag {extending map-trait} ;; XXX make sure map-trait methods are OK
      (to _.none?              map.none?)
      (to _.count              map.count)
      (to _.keys               map.keys)
      (to _.values             map.values)
      (to _.items              map.items)
      (to (_ .maps? key)       (map .maps? key))
      (to (_ key)              (map .get key 0)) ; TODO (map key) would be safer... revisit this
      (to (_ .get key default) (map .get key default))
      (to (_ .get key)         (map .get key))
      (to (_ .add! key)        (map .set! key
                                    (+ (map .get key 0) 1)))
      (to (_ .add-all! vals)   (for each! ((v vals))
                                 (bag .add! v)))
      (to (_ .delete! key)     (map .delete! key))  ; TODO should this decrement the value instead? Or should that be a different method?
      (to _.clear!             map.clear!)
      (to _.total              (sum map.values))
      ;; XXX fill in rest of bag interface (just the map interface, I guess)
      (to (_ .selfie sink)
        (sink .display "#<bag (")
        (sink .write map.count)
        (sink .display ")>"))
      ))

  (to (_ vals)
    (let bag (bag<-))     ;; TODO I couldn't use 'hey' here -- just a bootstrap problem?
    (bag .add-all! vals)
    bag))

(export bag<-)
