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

(let eg1
  '((9 #\z)
    (10 #\j)
    (20 #\q)
    (20 #\x)
    (52 #\k)
    (93 #\v)
    (161 #\g)
    (162 #\b)
    (188 #\y)
    (203 #\w)
    (225 #\m)
    (228 #\f)
    (229 #\p)
    (310 #\u)
    (320 #\c)
    (365 #\d) 
    (403 #\l)
    (514 #\h)
    (603 #\r)
    (659 #\s)
    (718 #\i)
    (719 #\n)
    (794 #\o)
    (805 #\a)
    (959 #\t)
    (1231 #\e)))

(to (main _)
  (show-tree (build-tree eg1)))
