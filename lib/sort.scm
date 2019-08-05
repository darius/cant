;; Generic sorting

(make sort
  (to `(,xs)
    (sort-by identity xs))
  (to `(,xs {reverse})          ;TODO design a better keyword-args scheme
    ;; TODO sort by 'negation' of key instead, but allowing for
    ;; non-numbers. Make up a negation-wrapper type?
    (reverse (sort-by identity xs))) 
  ;; ...
  )

(to (sort-by key<- sequence)

  (to (merge-sort seq)
    (begin splitting ((seq seq) (xs '()) (ys '()))
      (if seq.empty?
          (if xs.empty?
              ys
              (merge (merge-sort xs) (merge-sort ys)))
          (splitting seq.rest ys (link seq.first xs)))))

  (to (merge xs ys)
    (hm (if xs.empty? ys)
        (if ys.empty? xs)
        (if (<= (key<- xs.first) (key<- ys.first))
            `(,xs.first ,@(merge xs.rest ys)))
        (else
            `(,ys.first ,@(merge xs ys.rest)))))

  (merge-sort sequence))

(export
  sort sort-by)
