;; Growable mutable vectors (have a better name?)
;; TODO: shrink capacity sometimes

(to (fillvector<- @arguments)
  (fillvector<-vector (call vector<- arguments)))

(to (fillvector<-count start-count start-value)
  (fillvector<-vector (vector<-count start-count start-value)))

(to (fillvector<-vector start-vector)
  (let count (box<- start-vector.count))
  (let vec   (box<- start-vector))

  (to (grow)
    (let old vec.^)
    (let n old.count)
    (vec .^= (vector<-count (if (= 0 n) 1 (* 2 n))))
    (vec.^ .copy! old))

  (to (count-check i)
    (unless (< i count.^)
      (error "Bad index" fillvector i)))

  (make fillvector {extending vector-trait}
    ((i)
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
       (error "Underflow" fillvector))
     (count .^= i)
     (vec.^ i))
    ({.snapshot}
     (vec.^ .slice 0 count.^))         ;XXX make immutable
    ({.copy! v lo bound}
     (count-check bound)
     (vec.^ .copy! v lo bound))
    ({.extend! values}
     (for each! ((v values))
       (fillvector .push! v)))
    ({.selfie sink}
     (sink .display "#<fillvector (")
     (sink .print count.^)
     (sink .display ")>"))     
    ({.resize! n}
     (let old vec.^)
     (when (< old.count n) ;; TODO maybe we should always change the physical size
       (let new (vector<-count n))
       (new .copy! old 0 (min old.count n))
       (vec .^= new))
     (count .^= n))

    ({.copy! v}        (fillvector .copy! v 0 v.count))
    ({.slice lo}       (fillvector .slice lo count.^))
    ;; inefficient:
    ({.chain v}        (fillvector.snapshot .chain v))
    ({.slice lo bound} (fillvector.snapshot .slice lo bound))
    ))

(export fillvector<- fillvector<-count)
