;; Generic sorting
;; XXX untested
t
(make sort
  ((xs)
   (merge-sort xs compare))
  ((xs {reverse})            ;TODO design a better keyword-args scheme
   (merge-sort xs compare-reversed))
  ;; ...
  )

(define (compare x y)
  (x .compare y))                       ;TODO implement

(define (compare-reversed x y)
  (y .compare x))

(define (merge-sort sequence cmp)

  (define (sort seq)
    (begin splitting ((seq seq) (xs '()) (ys '()))
      (case (seq.empty?
             (if xs.empty?
                 ys
                 (merge (sort xs) (sort ys))))
            (else (splitting seq.rest ys (cons seq.first xs))))))

  (define (merge xs ys)
    (case (xs.empty? ys)
          (ys.empty? xs)
          (else (if (<= (cmp xs.first ys.first) 0)
                    `(,xs.first ,@(merge xs.rest ys))
                    `(,ys.first ,@(merge xs ys.rest))))))

  ;; TODO convert to list so .rest is efficient?
  (sort sequence))
