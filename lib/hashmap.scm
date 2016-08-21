;; Hash-maps

;; TODO:
;;   nonlinear probing -- how about xor probing?
;;   preserving insertion order
;;   deletion
;;   immutable snapshots
;;
;;   impl without a million boxes
;;   N.B. impl needs shared closures for efficiency
;;        (capacity, occupants, ..., hashmap)
;;   special-case impls for small maps and common-typed maps
;;   store hash codes instead of recomputing?

(let none (make))

(define (hash-map<-)
  (let count (box<- 0))
  (let keys  (box<- (vector<- none)))  ;; size a power of 2
  (let vals  (box<- (vector<- #no)))   ;; same size

  (define (capacity) keys.^.count)

  (define (occupants)
    (begin walking ((i (- (capacity) 1)))
      (if (< i 0)
          '()
          (do (let k (keys.^ i))
              (if (= k none)
                  (walking (- i 1))
                  (cons i (walking (- i 1))))))))

  (define (find key)
    (let mask (- keys.^.count 1))
    (let i0   (mask .and (__hash key)))
    (begin walking ((i i0))
      (let k (keys.^ i))
      (case ((= k none) {missing-at i})
            ((= k key)  {at i})
            (else
             (let j (mask .and (- i 1)))
             (if (= j i0)
                 (error "Can't happen")
                 (walking j))))))

  (define (maybe-grow)
    (when (< (* 2 (capacity))
             (* 3 count.^))
      (resize (* 2 (capacity)))))

  (define (resize new-capacity)
    (let old-keys keys.^)
    (let old-vals vals.^)
    (keys .^= (vector<-count new-capacity none))
    (vals .^= (vector<-count new-capacity))
    (for each! (((i key) old-keys.items))
      (unless (= key none)
        (let {missing-at j} (find key))
        (keys.^ .set! j key)
        (vals.^ .set! j (old-vals i)))))
                            
  (make hashmap
    ((key)
     (match (find key)
       ({at i} (vals.^ i))
       (_      (error "Missing key" hashmap key))))
    ({.get key}
     (hashmap .get key #no))
    ({.get key default}
     (match (find key)
       ({at i} (vals.^ i))
       (_      default)))
    ({.set! key val}
     (match (find key)
       ({at i}
        (vals.^ .set! i val))
       ({missing-at i}
        (keys.^ .set! i key)
        (vals.^ .set! i val)
        (count .^= (+ count.^ 1))
        (maybe-grow))))
    ({.maps? key}
     (match (find key)
       ({at _} #yes)
       (_      #no)))
    ({.empty?} (= count.^ 0))
    ({.count}  count.^)
    ({.keys}   (each keys.^ (occupants))) ;XXX lazy-map
    ({.values} (each vals.^ (occupants)))
    ({.items}
     (let ks keys.^)
     (let vs vals.^)
     (for each ((i (occupants)))
       `(,(ks i) ,(vs i))))
    ({.find? value}
     (hashmap.values .find? value))
    ({.find value}
     (let vs vals.^)
     (begin searching ((js (occupants)))  ;XXX should be lazy
       (case (js.empty? (error "Missing key" value))
             ((= value (vs js.first)) js.first)
             (else (searching js.rest)))))
    ({.selfie sink}
     (sink .display "#<hash-map (")
     (sink .print count.^)
     (sink .display ")>"))
    ))

(let map<- hash-map<-)

(define (map<-a-list a-list) ;TODO invent a concise constructor; frozen by default
  (let m (map<-))
  (for each! (((k v) a-list))
    (m .set! k v))
  m)

(export map<- map<-a-list)
