;; Game of Life again.
;; Represent a grid as a set of populated locations.
;; A location is an `(,x ,y) coordinate pair.

(import (use "lib/bag") bag<-)

(to (neighbors `(,x ,y))
  (for gather ((dx '(-1 0 1)))
    (for filter ((dy '(-1 0 1)))
      (and (not= `(,dx ,dy) '(0 0))
           `(,(+ x dx) ,(+ y dy))))))

(to (update grid)
  (let active (call bag<- (gather neighbors grid.keys)))
  (call set<-
        (for filter ((`(,pos ,n-live) active.items))
          (match n-live
            (3 pos)
            (2 (and (grid .get pos) pos))
            (_ #no)))))

(to (paint lines)
  (call set<- (for gather ((`(,row ,line) lines.items))
                (for filter ((`(,col ,ch) line.items))
                  (and (not= ch #\space)
                       `(,row ,col))))))

(let r-pentomino (paint '(" **"
                          "** "
                          " * ")))

(export update paint r-pentomino)
