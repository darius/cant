;; Growable mutable arrays (have a better name?)
;; TODO: shrink capacity sometimes

(to (flexarray<- @arguments)
  (flexarray<-array (array<-list arguments)))

(to (flexarray<-list arguments)
  (flexarray<-array (array<-list arguments)))

(to (flexarray<-count start-count start-value)
  (flexarray<-array (array<-count start-count start-value)))

(to (flexarray<-array start-array)
  (let count (box<- start-array.count))
  (let vec   (box<- start-array))

  (to (grow)
    (let old vec.^)
    (let n old.count)
    (vec .^= (array<-count (if (= 0 n) 1 (* 2 n))))
    (vec.^ .copy! old))

  (to (count-check i)
    (unless (< i count.^)               ;TODO check negative too
      (error "Bad index" flexarray i)))

  (make flexarray {extending array-trait}
    (to (_ i)
      (count-check i)
      (vec.^ i))
    (to _.count
      count.^)
    (to (_ .set! i value)
      (count-check i)
      (vec.^ .set! i value))
    (to (_ .push! value)
      (let i count.^)
      (when (= i vec.^.count) (grow))
      (vec.^ .set! i value)
      (count .^= i.+)
      i)                              ; (should we be returning this?)
    (to _.pop!
      (let i count.^.-)
      (when (< i 0)
        (error "Underflow" flexarray))
      (count .^= i)
      (vec.^ i))
    (to (_ .pop! i)
      (when (< i 0)
        (error "Out of range" flexarray))
      (count-check i)
      (let popped (vec.^ i))
;     (vec.^ .move! i (+ i 1) (- count.^ (+ i 1)))
      (__vector-move! vec.^ i vec.^ i.+ count.^)
      (count .update _.-)
      popped)
    (to (_ .insert! i value)                 ;TODO code duplication .push!
      (let c count.^)
      (unless (<= i c)
        (error "Bad index" flexarray i))
      (when (= c vec.^.count) (grow))
      (__vector-move! vec.^ i.+ vec.^ i c)
;     (vec.^ .move! (+ i 1) i (- c i))
      (vec.^ .set! i value)
      (count .^= c.+)
      i)                                 ;TODO what's a good return value?
    (to _.values ;; more efficient than array-trait's
      (each vec.^ (range<- count.^)))
    (to _.snapshot
      (vec.^ .slice 0 count.^))         ;XXX make immutable
    (to (_ .move! dst source lo bound)
      (when (< lo bound)
        (count-check (+ dst (- bound lo) -1))
        (vec.^ .move! dst source lo bound)))
    (to (_ .extend! values)
      (for each! ((v values))
        (flexarray .push! v)))
    (to (_ .selfie sink)
      (sink .display "#<flexarray (")
      (sink .write count.^)
      (sink .display ")>"))     
    (to (_ .resize! n)
      (let old vec.^)
      (when (< old.count n) ;; TODO maybe we should always change the physical size
        (let new (array<-count n))
        (new .move! 0 old 0 (min old.count n))
        (vec .^= new))
      (count .^= n))
    (to _.clear!
      (count .^= 0))                     ;TODO also reset vec?

    ;; inefficient:
    (to (_ .chain v)        (flexarray.snapshot .chain v))
    (to (_ .slice lo bound) (flexarray.snapshot .slice lo bound))
    ))

(export
  flexarray<- flexarray<-count flexarray<-list)
