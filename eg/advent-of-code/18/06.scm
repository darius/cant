;; This version, for part 2: breadth-first search for the frontier,
;; starting from the centroid (hoping that's within the area).

(import (use "eg/advent-of-code/utils")
  simple-parser<- average filter/lazy)
(import (use "lib/queue")
  empty empty?
  push extend
  peek)

(let margin 10000)
(let input (with-input-file '.read-lines "eg/advent-of-code/18/data/advent06"))
;(let margin 32)
;(let input (with-input-file '.read-lines "eg/advent-of-code/18/data/advent06.test"))

(let parser (simple-parser<- ":nat ', ' :nat"))

(to (parse coords)
  ('.results (parser coords)))

(let centers (each parse input))
(each! print centers)

(let names (map<- (zip centers (for each ((i (range<- centers.count)))
                                 (char<- (+ 65 32 i)))))) ;XXXchar-range<- again

(to (bounds<- points)
  (transpose (each bounds-1d<- (transpose points))))

(to (bounds-1d<- ns)
  `(,(call min ns) ,(call max ns)))

(let `((,xl ,yl) (,xh ,yh)) (bounds<- centers))
(format "bounds ~w\n" (bounds<- centers))

(to (show-map)
  (for each! ((y (yl .up-to yh)))
    (for each! ((x (xl .up-to xh)))
      (let p `(,x ,y))
      (let name (names .get p))
      (display (or name (if (included? p) #\# #\.))))
    (newline)))

(to (included? p)
  (< (total-distance p) margin))

(to (total-distance p)
  (sum (for each ((c centers))
         (distance<- c p))))

(to (distance<- `(,x0 ,y0) `(,x1 ,y1))
  (+ (abs (- x1 x0))
     (abs (- y1 y0))))


(display "\nPart 2\n")

;;(show-map)

;; This would take like a week for the non-test question.
;; But the code can help us test its successor.
(let area-1 (sum
             '()))
;;             (for each ((y ((- yl margin) .up-to (+ yh margin))))
;;               (sum (for each ((x ((- xl margin) .up-to (+ xh margin))))
;;                      (if (included? `(,x ,y)) 1 0)))))) ;TODO duplication

(to (integer<- n)
  (exact<-inexact (floor n)))

(to (wtf x)
  (format "wtf: ~w\n" x)
  x)

(let centroid (each (compose integer<- average)
                    (transpose centers)))
(format "centroid included?: ~w\n" (included? centroid))

(let region (set<-))

(to (grow-from seed)
  (let already (set<-))
  (begin growing ((queue (extend empty `(,seed))))
    (match (peek queue)
      ({empty})
      ({nonempty p queue-1}
;       (format "pop ~w ~w\n" p queue-1)
       (if (region p)
           (format "dupe ~w\n" p)
           (do (region .add! p)
               (format "add ~w distance ~w count ~w\n" p (total-distance p) region.count)))
       (growing (extend queue-1 
                        (for those ((p1 (neighbors<- p)))
                          (and (not (already p1))
                               (do (already .add! p1)
                                   (included? p1))))))))))

(to (neighbors<- `(,x ,y))
  `((,(- x 1) ,y)
    (,(+ x 1) ,y)
    (,x ,(- y 1))
    (,x ,(+ y 1))))

(to (neighbors-8<- `(,x ,y))
  (chain (neighbors<- `(,x ,y))
         `((,(- x 1) ,(- y 1))
           (,(+ x 1) ,(- y 1))
           (,(- x 1) ,(+ y 1))
           (,(+ x 1) ,(+ y 1)))))

(let seed
  (begin seeking ((p centroid))
    (let d (total-distance p))
    (format "seeking at ~w, distance ~w\n" p d)
    (if (< d margin)
        p
        (seeking (min-by total-distance (neighbors-8<- p))))))

'(let seed
  ('.first 
   (for gather/lazy ((y (range<- (- yl 0) (+ yh margin) 10)))
     (let row
       (for filter ((x ((- xl margin) .up-to (+ xh margin))))
         (let p `(,x ,y))
         `(,(total-distance p) ,p)))
     (let `(,best-d ,p) (call min row))
     (format "checking for seed, next row: ~w distance ~w\n" p best-d)
     (if (< best-d margin)
         `(,p)
         '()))))

(format "centroid ~w, seed ~w\n" centroid seed)

(grow-from seed)

;;(let area-2 (sum
;;             (for each ((y ((- (seed 1) margin) .up-to (+ (seed 1) margin))))
;;               (format "y ~w of ~w\n" y (+ (seed 1) margin))
;;               (sum (for each ((x ((- (seed 0) margin) .up-to (+ (seed 0) margin))))
;;                      (if (included? `(,x ,y)) 1 0))))))

;(format "result 2 (slow): ~w\n" area-1)
(format "result 2: ~w\n" region.count)
