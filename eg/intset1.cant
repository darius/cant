;; An example from William Cook's essay on OOP vs. ADTs
;; http://www.cs.utexas.edu/~wcook/Drafts/2009/essay.pdf

(make empty-set
  (to _.empty?      #yes)
  (to (_ .has? _)   #no)
  (to (_ .adjoin k) (adjoin<- k empty-set))
  (to (_ .merge s)  s))

(to (adjoin<- n s)
  (if (s .has? n)
      s
      (make extension
        (to _.empty?      #no)
        (to (_ .has? k)   (or (= n k) (s .has? k)))
        (to (_ .adjoin k) (adjoin<- k extension))
        (to (_ .merge s)  (merge<- extension s)))))

(to (merge<- s1 s2)
  (make meld
    (to _.empty?      (and s1.empty? s2.empty?))
    (to (_ .has? k)   (or (s1 .has? k) (s2 .has? k)))
    (to (_ .adjoin k) (adjoin<- k meld))
    (to (_ .merge s)  (merge<- meld s))))

;; Smoke test

(hide
 (let eg ((empty-set .adjoin 6) .adjoin 5))

 (print (eg .has? 5))
 (print (eg .has? 6))
 (print (eg .has? 7))
)
