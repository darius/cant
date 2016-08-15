;; Hash-maps

;; TODO:
;;   nonlinear probing -- how about xor probing?
;;   preserving insertion order
;;   deletion
;;   define a special None value like Python?
;;   immutable snapshots
;;   hash-sets
;;
;;   impl without a million boxes
;;   N.B. impl needs shared closures for efficiency
;;        (capacity, occupants, ..., hashmap)
;;   special-case impls for small maps and common-typed maps
;;   store hash codes instead of recomputing?

(let vacant (make))  ; (was 'empty', but renamed because global name clash)

(define (hash-map<-)
  (let count (box<- 0))
  (let keys  (box<- (vector<- vacant)))  ;; size a power of 2
  (let vals  (box<- (vector<- #no)))     ;; same size

  (define (capacity) keys.^.count)

  (define (occupants)
    (begin walking ((i (- (capacity) 1)))
      (if (< i 0)
          '()
          (do (let k (keys.^ i))
              (if (= k vacant)
                  (walking (- i 1))
                  (cons i (walking (- i 1))))))))

  (define (find key succeed fail)
    (let h    (__hash key))
    (let mask (- keys.^.count 1))
    (let i0   (mask .and h))
    (begin walking ((i i0))
      (let k (keys.^ i))
      (case ((= k vacant)
             (fail i))
            ((= k key)
             (succeed i))
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
    (keys .^= (vector<-count new-capacity vacant))
    (vals .^= (vector<-count new-capacity))
    (for each! ((i (range<- old-keys.count)))
      (let key (old-keys i))
      (unless (= key vacant)
        (find key
              (given (j) (error "Can't happen"))
              (given (j)
                (keys.^ .set! j key)
                (vals.^ .set! j (old-vals i)))))))
                            
  (make hashmap
    ({.keys}   (each keys.^ (occupants))) ;XXX lazy-map
    ({.values} (each vals.^ (occupants)))
    ({.items}
     (let ks keys.^)
     (let vs vals.^)
     (for each ((i (occupants)))
       `(,(ks i) ,(vs i))))
    ({.empty?} (= count.^ 0))
    ({.count}  count.^)
    ((key)
     (find key vals.^ (given (_) (error "Missing key" hashmap key))))
    ({.get key}
     (hashmap .get key #no))
    ({.get key default}
     (find key vals.^ (given (_) default)))
    ({.set! key val}
     (find key
           (given (i)
             (vals.^ .set! i val))
           (given (i)
             (keys.^ .set! i key)
             (vals.^ .set! i val)
             (count .^= (+ count.^ 1))
             (maybe-grow))))
    ({.maps? key}
     (find key
           (given (_) #yes)
           (given (_) #no)))
    ({.maps-to? value}
     (hashmap.values .maps-to? value))
    ({.find-key-for value}
     (unimplemented))                   ;XXX
    ({.selfie sink}
     (sink .display "#<hash-map (")
     (sink .print hashmap.count)
     (sink .display ")>"))
    ))

(let map<- hash-map<-)

(define (map<-a-list a-list) ;TODO invent a concise constructor; frozen by default
  (let m (map<-))
  (for each! (((k v) a-list))
    (m .set! k v))
  m)

(newline)

(hide
 (let a (hash-map<-))
 (print a)
 (print (a .get 42))
 (a .set! 'x "yay")
 (print a)
 (print (a .get 'x))
 (a .set! 'x "boo")
 (print a)
 (print (a .get 'x))
 (print (a .get 'y 'nope))

 (a .set! 'z "zeee")
 (print a)
 (print (a .get 'x))
 (print (a .get 'y))
 (print (a .get 'z))
 (print (list<- a.keys a.values a.items a.empty? a.count))
 (print (a 'z))

 ;; TODO more tests

 (define (random-tests n-trials)
   (for each! ((_ (range<- n-trials)))
        (exercise-em (for each ((value (range<- 50)))
                          (let key (random-integer 16))
                          (let op  (if (< (random-integer 10) 3)
                                       'fetch
                                       value))
                          `(,key ,op)))))

 (define (exercise-em pairs)
   (let m (map<-))     ;; The hashmap under test.
   (let a (box<- '())) ;; An a-list that should be equivalent.
   (for each! (((key op) pairs))
        (match op
               ('fetch
                (let m-val (m .get key))
                (let a-val (match (assv key a.^)
                                  (#no #no)
                                  ((k v) v)))
                (assert (= m-val a-val))
                (print `(,key ,m-val))
                )
               (value
                (m .set! key value)
                (a .^= `((,key ,value) ,@a.^))
                ;; TODO test equivalence here
                ))))

 ;;(random-tests 400)
)
