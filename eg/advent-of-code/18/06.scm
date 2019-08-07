;; (Use run.scm to run this.)

;; This version, for part 2: breadth-first search for the frontier,
;; starting from the centroid (hoping that's within the area).

(import (use 'queue)
  empty empty?
  push extend
  peek)

(let margin (may data-name
              (be "06.in" 10000)
              (be "06.test" 32)))
(let input (with-input-file _.read-lines data-file))

(let parse (simple-parser<- ":nat ', ' :nat"))
(let centers (each parse input))

(let names (map<- (zip centers
                       (#\a .span centers.count))))

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
  (tally included? (grid* ((- xl margin) .to (+ xh margin))
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
    (may (peek queue)
      (be {empty})
      (be {nonempty p queue-1}
        (if (region .maps? p)
            (format "dupe ~w\n" p)
            (do (region .add! p)
;;               (format "add ~w distance ~w count ~w\n" p (total-distance p) region.count)
                ))
        (growing (extend queue-1 
                         (for those ((p1 (neighbors<- p)))
                           (and (not (already .maps? p1))
                                (do (already .add! p1)
                                    (included? p1))))))))))

(to (neighbors<- `(,x ,y))
  `((,x.- ,y)
    (,x.+ ,y)
    (,x ,y.-)
    (,x ,y.+)))

(grow-from centroid)

;; Detritus from the bug-hunt

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
;;                      (_.count (included? `(,x ,y))))))))

;(format "result 2 (slow): ~w\n" (area-1))

(format "result 2: ~w\n" region.count)
