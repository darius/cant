;; This version, for part 2: breadth-first search for the frontier,
;; starting from the centroid (hoping that's within the area).

(import (use "eg/advent-of-code/utils")
  simple-parser<- average bounds<- manhattan-distance<-)
(import (use "lib/queue")
  empty empty?
  push extend
  peek)

(let margin 10000)
(let input (with-input-file '.read-lines "eg/advent-of-code/18/data/advent06"))
;(let margin 32)
;(let input (with-input-file '.read-lines "eg/advent-of-code/18/data/advent06.test"))

(let parse (simple-parser<- ":nat ', ' :nat"))
(let centers (each parse input))

(let names (map<- (zip centers
                       (#\a .to< (+ #\a centers.count)))))

(let `((,xl ,yl) (,xh ,yh)) (bounds<- centers))
(format "bounds ~w\n" (bounds<- centers))

(to (show-map)
  (for each! ((y (yl .to yh)))
    (for each! ((x (xl .to xh)))
      (let p `(,x ,y))
      (display (or (names .get p)
                   (if (included? p) #\# #\.))))
    (newline)))

(to (included? p)
  (< (total-distance p) margin))

(to (total-distance p)
  (sum (for each ((c centers))
         (manhattan-distance<- c p))))


(display "\nPart 2\n")

;;(show-map)

;; This would take like a week for the non-test question.
;; But the code can help us test its successor.
(to (area-1)
  (tally included? (product<- ((- xl margin) .to (+ xh margin))
                              ((- yl margin) .to (+ yh margin)))))

(to (integer<- n)
  (exact<-inexact (floor n)))

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
       (if (region p)
           (format "dupe ~w\n" p)
           (do (region .add! p)
;;               (format "add ~w distance ~w count ~w\n" p (total-distance p) region.count)
               ))
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

(grow-from centroid)

;; Detritus from the bug-hunt

;; (to (neighbors-8<- `(,x ,y))
;;   (chain (neighbors<- `(,x ,y))
;;          `((,(- x 1) ,(- y 1))
;;            (,(+ x 1) ,(- y 1))
;;            (,(- x 1) ,(+ y 1))
;;            (,(+ x 1) ,(+ y 1)))))

;; (let seed
;;   (begin seeking ((p centroid))
;;     (let d (total-distance p))
;;     (format "seeking at ~w, distance ~w\n" p d)
;;     (if (< d margin)
;;         p
;;         (seeking (min-by total-distance (neighbors-8<- p))))))

;; (format "centroid ~w, seed ~w\n" centroid seed)

;; (grow-from seed)

;; That's interesting: grow-from centroid was considerably slower than grow-from seed.
;; Why? They're both in the `included?` area.
;; Hm, no, I guess the slowdown was from using the more generic manhattan-distance<-.
;; Change one thing at a time.

;;(let area-2 (sum
;;             (for each ((y ((- (seed 1) margin) .to (+ (seed 1) margin))))
;;               (format "y ~w of ~w\n" y (+ (seed 1) margin))
;;               (sum (for each ((x ((- (seed 0) margin) .to (+ (seed 0) margin))))
;;                      (if (included? `(,x ,y)) 1 0))))))

;(format "result 2 (slow): ~w\n" (area-1))

(format "result 2: ~w\n" region.count)
