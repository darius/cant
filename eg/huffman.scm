;; Huffman coding

(to (build-tree freqs)
  (let leaves (sort (for each (((frequency symbol) freqs))
                      `(,frequency {leaf ,symbol}))))
  ;; TODO use pairing-heap
  (begin building ((trees leaves))
    (match trees
      (((_ t1)) t1)
      (((f1 t1) (f2 t2) @rest)
       (building (insert `(,(+ f1 f2) {branch ,t1 ,t2})
                         rest))))))

(to (insert x xs)
  (match xs
    (() `(,x))
    ((hd @tl)
     (if (<= x hd)
         `(,x ,@xs)
         `(,hd ,@(insert x tl))))))

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
