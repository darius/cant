;; Starting from 17.scm, starting over on the flooding code.

(import (use "eg/advent-of-code/utils")
  simple-parser<- vector+ bounds<-)
(import (use "lib/grid-2d")
  grid-2d<-)

;(let input (with-input-file '.read-lines "eg/advent-of-code/18/data/advent17.test"))
(let input (with-input-file '.read-lines "eg/advent-of-code/18/data/advent17"))

(let parse
  (simple-parser<- ":anyone '=' :int ', ' :anyone '=' :int '..' :int"))

(let inputs (each parse input))


(let spring '(500 0))

(let clay-spots
  (for gather ((`(,var1 ,val1 ,var2 ,lo ,hi) inputs))
    (match `(,var1 ,var2)
      ('("x" "y") (for each ((j (lo .to hi)))
                    `(,val1 ,j)))
      ('("y" "x") (for each ((j (lo .to hi)))
                    `(,j ,val1))))))

(let `((,xl0 ,yl) (,xh0 ,yh)) (bounds<- clay-spots))

;; Extend the x-bounds by 1 each way to allow for flow off the sides.
(let xl (- xl0 1))
(let xh (+ xh0 1))

(let grid (grid-2d<- `(,xl ,yl) `(,xh ,yh) {constant #no}))
(for each! ((p clay-spots))
  (grid .set! p #\#))

(to (escaped? `(,_ ,y))
  (< yh y))

(to (clay? p)
  (= (grid .get p) #\#))

(to (show-map)
  (for each! ((y ((spring 1) .to< yl)))
    (for each! ((x (xl .to xh)))
      (let p `(,x ,y))
      (display (if (= spring p) #\+ #\.)))
    (newline))
  (grid .show (given (chars)
                (for each! ((ch chars))
                  (display (or ch #\.)))
                (newline)))
  (newline))

;; (show-map)

(to (under p)
  (vector+ p '(0 1)))

(to (flood)
  (let under-spring `(,(spring 0) ,yl))
  (surely (<= (spring 1) yl))
  (surely (not (clay? under-spring)))  ;; gonna assume this for simplicity
  (flooding under-spring))

(to (blocked? p)
  ("#~" .find? (grid .get p)))

(to (flooding p)
  (unless (escaped? p)
    (surely (not (grid .get p)) "Only flood into openings")
    (grid .set! p #\|)
    (let below (under p))
    (unless (grid .get below)
      (flooding below))
    (when (blocked? below)
      ;; Spill to the sides.
      (let `(,x0 ,y0) p)
      (to (spilling x dx)
        (let x1 (+ x dx))
        (let next `(,x1 ,y0))
        (match (grid .get next)
          (#no
           (grid .set! next #\|)
           (when (not (grid .get (under next)))
             (flooding (under next)))
           (if (blocked? (under next))
               (spilling x1 dx)
               x))
          (#\| x)
          (#\# x)
          (#\~ (surely #no "Can't happen, can it?"))
          (_   (surely #no "Even less happenable"))))
      (let x-min (spilling x0 -1))
      (let x-max (spilling x0 1))
      (let span (for each ((x (x-min .to x-max)))
                  `(,x ,y0)))
      (when (and (every (compose blocked? under) span)  ;; TODO I think this logic is now performed above
                 (blocked? `(,(- x-min 1) ,y0))
                 (blocked? `(,(+ x-max 1) ,y0)))
        (for each! ((spot span))
           (surely (= #\| (grid .get spot)) "Converting flowing to still water")
           (grid .set! spot #\~))))))


(do
  (flood)
  (let bag (call bag<- grid.values))
  (format "Total water (part 1): ~w\n" (+ (bag .get #\~ 0) (bag .get #\| 0)))
  (format "Still water (part 2): ~w\n" (bag .get #\~)))
