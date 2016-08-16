(make not-yet)

(define (memoize f)
  (let memos (map<-))
  (define (memoized @arguments)
    (let recalled (memos .get arguments not-yet))
    (if (= recalled not-yet)
        (do (let computed (call f arguments))
            (memos .set! arguments computed)
            computed)
        recalled)))

(export memoize)
