;; An example from William Cook's essay on OOP vs. ADTs
;; http://www.cs.utexas.edu/~wcook/Drafts/2009/essay.pdf

(load "stdlib.scm")

(define empty
  (make
    ('empty? ()  #t)
    ('has? (k)   #f)
    ('adjoin (k) (adjoin<- k empty))
    ('merge (s)  s)))

(define (adjoin<- n s)
  (if ('has? s n)
      s
      (make extension
        ('empty? ()  #f)
        ('has? (k)   (if ('= n k) #t ('has? s k)))
        ('adjoin (k) (adjoin<- k extension))
        ('merge (s)  (merge<- extension s)))))

(define (merge<- s1 s2)
  (make meld
    ('empty? ()  (if ('empty? s1) ('empty? s2) #f))
    ('has? (k)   (if ('has? s1 k) #t ('has? s2 k)))
    ('adjoin (k) (adjoin<- k meld))
    ('merge (s)  (merge<- meld s))))

;; Smoke test

(define eg ('adjoin ('adjoin empty 6) 5))

(print ('has? eg 5))
(print ('has? eg 6))
(print ('has? eg 7))
