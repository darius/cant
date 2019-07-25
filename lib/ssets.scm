;; Immutable sorted sets

(to (sset<- @xs)
  (sort xs))

(to (sset<-list xs)
  (sort xs))

(to (sset-elements xs)
  xs)

(to (sset-union xs ys)
  (merge xs ys))

(to (sset-insert xs x)
  (merge `(,x) xs))

(to (sset-difference xs ys)
  (diff xs ys))

(to (sset-remove xs unwanted)
  (match xs
    ('() '())
    (`(,x1 ,@xs1)
     (if (= x1 unwanted)
         xs1
         (link x1 (sset-remove xs1 unwanted))))))

(to (merge xs ys)               ;TODO dedupe (extracted from sort.scm)
  (case (xs.empty? ys)
        (ys.empty? xs)
        (else (match (xs.first .compare ys.first)
                (-1 `(,xs.first ,@(merge xs.rest ys)))
                ( 0 `(,xs.first ,@(merge xs.rest ys.rest)))
                ( 1 `(,ys.first ,@(merge xs ys.rest)))))))

(to (diff xs ys)
  (case (xs.empty? '())
        (ys.empty? xs)
        (else (match (xs.first .compare ys.first)
                (-1 `(,xs.first ,@(diff xs.rest ys)))
                ( 0 (diff xs.rest ys.rest))
                ( 1 (diff xs ys.rest))))))

;(print (diff '(1 3 5 7) '(0 2 3 4 5 9)))

(export sset<- sset<-list sset-elements sset-insert sset-remove sset-union sset-difference)
