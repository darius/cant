(import (use "lib/ssets")  
  sset<- sset<-list sset-elements sset-insert sset-remove sset-union sset-difference)

(import (use "lib/squickcheck")
  a-claim a-nat an-int a-char a-printable-char a-printable-string a-list-of a-tuple a-choice
  weighted-choice
  all should  ;; I dunno what to call it yet
  )

(let s1 (sset<- 1 7 5 3))
(let s2 (sset<- 0 2 3 4 5 9))

(print (sset<- 1 1 1 1))
(print (sset-difference s1 s2))
(print (sset-difference s2 s1))

;;XXX
'(for all ((x (a-list-of a-nat)))
  (let sx (sset<-list x))
  (let mx (set<-list x))
  (format "hey ~w ~w\n" (sset-elements sx) (sort mx.keys))
  (should = (sset-elements sx) (sort mx.keys)))
;; TODO more
