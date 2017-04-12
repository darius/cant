;; Growable mutable arrays (have a better name?)
;; TODO: shrink capacity sometimes

(to (flexarray<- @arguments)
  (flexarray<-array (call array<- arguments)))

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
    (unless (< i count.^)
      (error "Bad index" flexarray i)))

  (make flexarray {extending array-trait}
    (`(,i)
     (count-check i)
     (vec.^ i))
    ({.count}
     count.^)
    ({.set! i value}
     (count-check i)
     (vec.^ .set! i value))
    ({.push! value}
     (let i count.^)
     (when (= i vec.^.count) (grow))
     (vec.^ .set! i value)
     (count .^= (+ i 1))
     i)                              ; (should we be returning this?)
    ({.pop!}
     (let i (- count.^ 1))
     (when (< i 0)
       (error "Underflow" flexarray))
     (count .^= i)
     (vec.^ i))
    ({.snapshot}
     (vec.^ .slice 0 count.^))         ;XXX make immutable
    ({.copy! v lo bound}
     (count-check bound)
     (vec.^ .copy! v lo bound))
    ({.extend! values}
     (for each! ((v values))
       (flexarray .push! v)))
    ({.selfie sink}
     (sink .display "#<flexarray (")
     (sink .print count.^)
     (sink .display ")>"))     
    ({.resize! n}
     (let old vec.^)
     (when (< old.count n) ;; TODO maybe we should always change the physical size
       (let new (array<-count n))
       (new .copy! old 0 (min old.count n))
       (vec .^= new))
     (count .^= n))
    ({.clear!}
     (count .^= 0))                     ;TODO also reset vec?

    ;; inefficient:
    ({.chain v}        (flexarray.snapshot .chain v))
    ({.slice lo bound} (flexarray.snapshot .slice lo bound))
    ))

(export
  flexarray<- flexarray<-count)
