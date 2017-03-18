(import (use "lib/hashset") set<- union)

(let a (set<-))
(print ((union (set<- 1) (set<- 3)) .keys))
