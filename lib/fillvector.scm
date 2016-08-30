;; Growable mutable vectors (have a better name?)
;; TODO: shrink capacity sometimes

(define (fillvector<- @arguments)
  (fillvector<-vector (call vector<- arguments)))

(define (fillvector<-count start-count start-value)
  (fillvector<-vector (vector<-count start-count start-value)))

(define (fillvector<-vector start-vector)
  (let count (box<- start-vector.count))
  (let vec   (box<- start-vector))

  (define (grow)
    (let old vec.^)
    (let n old.count)
    (vec .^= (vector<-count (if (= 0 n) 1 (* 2 n))))
    (vec.^ .copy! old))

  (define (count-check i)
    (unless (< i count.^)
      (error "Bad index" fillvector i)))

  (make fillvector
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
    ({.selfie sink}
     (sink .display "#<fillvector (")
     (sink .print count.^)
     (sink .display ")>"))     

    ({.copy! v}        (fillvector .copy! v 0 v.count))
    ({.slice lo}       (fillvector .slice lo count.^))
    ;; inefficient:
    ({.chain v}        (fillvector.snapshot .chain v))
    ({.slice lo bound} (fillvector.snapshot .slice lo bound))

    ;; TODO there should be a vector trait
    (message           (list-trait fillvector message))))

(export fillvector<- fillvector<-count)
