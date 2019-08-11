;; Immutable sorted sets

(to (sset<- @xs)
  (sset<-list xs))

(to (sset<-list xs)
  (for foldr ((x xs) (ss '()))
    (sset-insert ss x)))

(to (sset-elements xs)
  xs)

(to (sset-union xs ys)
  (merge xs ys))

(to (sset-insert xs x)
  (merge `(,x) xs))

(to (sset-difference xs ys)
  (diff xs ys))

(to (sset-remove xs unwanted)
  (may xs
    (be '() '())
    (be (link x1 xs1)
      (if (= x1 unwanted)
          xs1
          (link x1 (sset-remove xs1 unwanted))))))

;; Not quite the same as sort.scm's merge, since we dedupe here.
(to (merge xs ys)
  (hm (if xs.none? ys)
      (if ys.none? xs)
      (else (may (xs.first .compare ys.first)
              (be -1 (link xs.first (merge xs.rest ys)))
              (be  0 (link xs.first (merge xs.rest ys.rest)))
              (be  1 (link ys.first (merge xs ys.rest)))))))

(to (diff xs ys)
  (hm (if xs.none? '())
      (if ys.none? xs)
      (else (may (xs.first .compare ys.first)
              (be -1 (link xs.first (diff xs.rest ys)))
              (be  0 (diff xs.rest ys.rest))
              (be  1 (diff xs ys.rest))))))

(export sset<- sset<-list sset-elements sset-insert sset-remove sset-union sset-difference)
