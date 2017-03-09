;; Return a memoized version of a function

(make not-yet)

(to (memoize f)
  (let memos (map<-))
  (to (memoized @arguments)
    (let recalled (memos .get arguments not-yet))
    (if (= recalled not-yet)
        (do (let computed (call f arguments))
            (memos .set! arguments computed)
            computed)
        recalled)))

(export memoize)
