;; Generic sorting

(make sort
  (`(,xs)
   (sort-by xs identity))
  (`(,xs {reverse})          ;TODO design a better keyword-args scheme
   ;; TODO sort by 'negation' of key instead, but allowing for
   ;; non-numbers. Make up a negation-wrapper type?
   (reverse (sort-by xs identity))) 
  ;; ...
  )

(to (sort-by sequence key<-)

  (to (merge-sort seq)
    (begin splitting ((seq seq) (xs '()) (ys '()))
      (if seq.empty?
          (if xs.empty?
              ys
              (merge (merge-sort xs) (merge-sort ys)))
          (splitting seq.rest ys (cons seq.first xs)))))

  (to (merge xs ys)
    (case (xs.empty? ys)
          (ys.empty? xs)
          (else (if (<= (key<- xs.first) (key<- ys.first))
                    `(,xs.first ,@(merge xs.rest ys))
                    `(,ys.first ,@(merge xs ys.rest))))))

  (merge-sort sequence))

(export
  sort sort-by)
