(import (use 'ssets)
  sset<- sset<-list sset-elements sset-insert sset-remove sset-union sset-difference)

(import (use 'squickcheck)
  a-nat a-list-of
  all should)

(let s1 (sset<- 1 7 5 3))
(let s2 (sset<- 0 2 3 4 5 9))

(print (sset<- 1 1 1 1))
(print (sset-difference s1 s2))
(print (sset-difference s2 s1))

(for all ((x (a-list-of a-nat)))
  (let sx (sset<-list x))
  (let mx (set<-list x))
  (should = (sset-elements sx) (sort mx.keys)))

(for all ((x (a-list-of a-nat))
          (y (a-list-of a-nat)))
  (let sx (sset<-list x)) (let sy (sset<-list y)) (let sr (sset-union sx sy))
  (let mx (set<-list x))  (let my (set<-list y))  (let mr (mx .union my))
  (should = (sset-elements sr) (sort mr.keys)))

;; TODO this is getting pretty clumsy
