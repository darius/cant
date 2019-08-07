;; (Use run.scm to run this.)

(import (use 'grid-2d)
  grid-2d<-)

(let input (with-input-file _.read-lines data-file))

(let parse
  (simple-parser<- ":anyone '=' :int ', ' :anyone '=' :int '..' :int"))

(let inputs (each parse input))


(let spring '(500 0))

(let clay-spots
  (for gather ((input inputs))
    (may input
      (be `("x" ,x "y" ,lo ,hi) (grid* `(,x) (lo .to hi)))
      (be `("y" ,y "x" ,lo ,hi) (grid* (lo .to hi) `(,y))))))

(let `((,xl0 ,yl) (,xh0 ,yh)) (bounds<- clay-spots))

;; Extend the x-bounds by 1 each way to allow for flow off the sides.
(let xl xl0.-)
(let xh xh0.+)

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
  (grid .show (on (chars)
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
        (may (grid .get next)
          (be #no
            (grid .set! next #\|)
            (when (not (grid .get (under next)))
              (flooding (under next)))
            (if (blocked? (under next))
                (spilling x1 dx)
                x))
          (be #\| x)
          (be #\# x)
          (be #\~ (surely #no "Can't happen, can it?"))
          (else   (surely #no "Even less happenable"))))
      (let x-min (spilling x0 -1))
      (let x-max (spilling x0 1))
      (let span (for each ((x (x-min .to x-max)))
                  `(,x ,y0)))
      (when (and (every (compose blocked? under) span)  ;; TODO I think this logic is now performed above
                 (blocked? `(,x-min.- ,y0))
                 (blocked? `(,x-max.+ ,y0)))
        (for each! ((spot span))
          (surely (= #\| (grid .get spot)) "Converting flowing to still water")
          (grid .set! spot #\~))))))


(do
  (flood)
  (let bag (bag<- grid.values))
  (format "Total water (part 1): ~w\n" (+ (bag #\~) (bag #\|)))
  (format "Still water (part 2): ~w\n" (bag #\~)))
