;; Huffman coding

(import (use "lib/pairing-heap") priority-queues<-)

(import (priority-queues<- <=)
  pq-empty? pq-min
  empty-pq unit-pq pq-merge pq-insert pq-remove-min)

(to (build-tree freqs)
  (let leaves (for each (((frequency symbol) freqs))
                `(,frequency {leaf ,symbol})))
  ;; TODO? define pq-merge-many or something
  (begin building ((pq (foldr pq-merge (each unit-pq leaves) empty-pq)))
    (let (f1 t1) (pq-min pq))
    (let rest (pq-remove-min pq))
    (if (pq-empty? rest)
        t1
        (do (let (f2 t2) (pq-min rest))
            (let combo `(,(+ f1 f2) {branch ,t1 ,t2}))
            ;; TODO? define pq-replace-min
            (building (pq-insert (pq-remove-min rest) combo))))))

(to (walk tree visit)
  (begin walking ((path '()) (tree tree))
    (match tree
      ({leaf symbol}
       (visit symbol (reverse path)))
      ({branch on-0 on-1}
       (walking `(0 ,@path) on-0)
       (walking `(1 ,@path) on-1)))))

(to (show-tree tree)
  (walk tree (given (symbol encoding)
               (format "~d ~d\n" symbol (string<-list (each "01" encoding))))))

(to (encoder<- tree)
  (let encoder (map<-))
  (walk tree (given (symbol encoding)
               (encoder .set! symbol encoding)))
  encoder)

(to (encode encoder symbols)
  (gather encoder symbols))

(to (decode tree bits)
  (begin stepping ((subtree tree) (bits bits))
    (match subtree
      ({leaf symbol}
       `(,symbol ,@(if bits.empty?
                       '()
                       (stepping tree bits))))
      ({branch @branches}
       (if bits.empty?
           '()       ;TODO: check that we're at the root?
           (stepping (branches bits.first) bits.rest))))))

(export build-tree show-tree encoder<- encode decode)
