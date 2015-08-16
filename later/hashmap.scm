;; Hash-maps
;; Chances are we'll want these to be primitive, for use by vtables at least.

(load "stdlib.scm")

;; TODO:
;;   define equal?, 'hash -- mirandizing
;;   nonlinear probing
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

(let empty (make))

(define (hashmap<-)
  (let count (box<- 0))
  (let keys  (box<- (vector<- empty)))  ;; size a power of 2
  (let vals  (box<- (vector<- #f)))     ;; same size

  (define (capacity) (.count (keys)))

  (define (occupants)
    (recurse walking ((i (- (capacity) 1)))
      (if (< i 0)
          '()
          (hide
           (let k ((keys) i))
           (if (is? k empty)
               (walking (- i 1))
               (cons i (walking (- i 1))))))))

  (define (find key succeed fail)
    (let h    (.hash key))              ;XXX coerce to fixnum
    (let mask (- (.count keys) 1))
    (let i0   (.bit-and mask h))
    (recurse walking ((i i0))
      (let k ((keys) i))
      (cond ((is? k empty)
             (fail i))
            ((equal? k key)             ;XXX
             (succeed i))
            (else
             (let j (.bit-and mask (- i 1)))
             (if (= j i0)
                 (error "Can't happen")
                 (walking j))))))

  (define (maybe-grow)
    (when (< (* 2 (capacity))
             (* 3 (count)))
      (resize (* 2 (capacity)))))

  (define (resize new-capacity)
    (let old-keys (keys))
    (let old-vals (vals))
    (.set! keys (vector<-count new-capacity empty))
    (.set! vals (vector<-count new-capacity))
    (for each! ((i (range<- (.count old-keys))))
      (let key (old-keys i))
      (unless (is? key empty)
        (find key
              (given (j) (error "Can't happen"))
              (given (j)
                (.set! (keys) j key)
                (.set! (vals) j (old-vals i)))))))
                            
  (make hashmap
    (.keys ()   (each (keys) (occupants))) ;XXX lazy-map
    (.values () (each (vals) (occupants)))
    (.items ()
      (let ks (keys))
      (let vs (vals))
      (for each ((i (occupants)))
        `(,(ks i) ,(vs i))))
    (.empty? () (is? (count) 0))
    (.count ()  (count))
    (.run (key)
      (find key (vals) (given (i) (error "Missing key" hashmap key))))
    (.get (key)
      (.get hashmap key #f))
    (.get (key default)
      (find key (vals) (given (i) default)))
    (.set! (key val)
      (find key
            (given (i)
              (.set! (vals) i val))
            (given (i)
              (.set! (keys) i key)
              (.set! (vals) i val)
              (maybe-grow))))
    ))
