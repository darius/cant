;; Return a memoized version of a function

(make not-yet)

(to (memoize f)
  (let memos (map<-))
  (to (memoized @arguments)
    ;; TODO what's the performance of turning this into a one-liner:
    ;; (memos .get-set! arguments (: (call f arguments)))
    ;; except of course .get-set! isn't quite the thing.
    (let recalled (memos .get arguments not-yet))
    (if (= recalled not-yet)
        (do (let computed (call f arguments))
            (memos .set! arguments computed)
            computed)
        recalled)))

(export memoize)
