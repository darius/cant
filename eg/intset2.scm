;; An example from William Cook's essay on OOP vs. ADTs
;; http://www.cs.utexas.edu/~wcook/Drafts/2009/essay.pdf
;; We don't yet support abstract datatypes (need 'synergy') --
;; we could actually hack that up in terms of =, but it'd be
;; nicer with a primitive.

(let empty-set {empty})

(define (adjoin<- n s)
  (if (has? s n)
      s
      {extension n s}))

(define (merge<- s1 s2)
  (match s1
    ({empty} s2)
    (_ {meld s1 s2})))

(define (empty? s)
  (match s
    ({empty}         #yes)
    ({extension _ _} #no)
    ({meld s1 s2}    (and (empty? s1) (empty? s2)))))

(define (has? s n)
  (match s
    ({empty}           #no)
    ({extension n1 s1} (or (= n n1) (has? s1 n)))
    ({meld s1 s2}      (or (has? s1 n) (has? s2 n)))))

;; Smoke test

(hide
 (let eg (adjoin<- 5 (adjoin<- 6 empty-set)))

 (print (has? eg 5))
 (print (has? eg 6))
 (print (has? eg 7))
)
