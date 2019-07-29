;; Huffman coding

(import (use 'pairing-heap) priority-queues<-)

(import (priority-queues<- <=)
  pq-empty? pq-min
  empty-pq unit-pq pq-merge pq-insert pq-remove-min)

(to (build-tree freqs)
  (let pqs (for each ((`(,frequency ,symbol) freqs))
             (unit-pq `(,frequency {leaf ,symbol}))))
  ;; TODO? define pq-merge-many or something
  (begin building ((pq (foldr pq-merge pqs empty-pq)))
    ;; TODO? define pq-dequeue or something
    (let `(,f1 ,t1) (pq-min pq))
    (let rest (pq-remove-min pq))
    (if (pq-empty? rest)
        t1
        (do (let `(,f2 ,t2) (pq-min rest))
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
  (walk tree (method<- encoder '.set!))
  encoder)

(to (encode encoder symbols)
  (gather encoder symbols))

(to (decode root bits)
  (begin stepping ((tree root) (bits bits))
    (if bits.empty?
        '()
        (do (let {branch @subtrees} tree)
            (match (subtrees bits.first)
              ({leaf symbol} `(,symbol ,@(stepping root bits.rest)))
              (subtree       (stepping subtree bits.rest)))))))

(export build-tree show-tree encoder<- encode decode)
