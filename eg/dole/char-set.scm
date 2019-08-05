;; Sets of characters

(to (char-set<- @chars)
  (let set (set<-list chars))
  (make char-set
    (to (_ .maps? ch) (set .maps? ch))
    ))
    
(export
  char-set<-)
