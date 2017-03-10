;; Sets of characters

(to (char-set<- @chars)
  (let set (set<- chars))
  (make char-set
    ({.maps? ch} (set .maps? ch))
    ))
    
(export
  char-set<-)
