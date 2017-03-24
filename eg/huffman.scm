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

(to (show-tree tree)
  (begin walking ((path '()) (tree tree))
    (match tree
      ({leaf symbol}
       (format "~d ~d\n" symbol (string<-list (reverse path))))
      ({branch on-0 on-1}
       (walking `(#\0 ,@path) on-0)
       (walking `(#\1 ,@path) on-1)))))

(export build-tree show-tree)
