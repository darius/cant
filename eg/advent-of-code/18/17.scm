;; (Use run.scm to run this.)

(import (use 'grid-2d)
  grid-2d<-)

(let input (with-input-file _.read-lines data-file))

(let parse
  (simple-parser<- ":anyone '=' :int ', ' :anyone '=' :int '..' :int"))

(let inputs (each parse input))


;;TODO reconcile use of lists and tuples

(to (tuple<-list xs)
  (term<- '_ xs))

(to (tuple-get tuple n)        ;not used, but it's gonna come up again
  (surely (and (term? tuple) (= tuple.tag '_)))
  (tuple.arguments n))


(let spring-x 500)
(let spring-y   0)
(let spring (_ spring-x spring-y))

(let clay-spots
  (for gather ((input inputs))
    (may input
      (be `("x" ,x "y" ,lo ,hi) (grid* `(,x) (lo .to hi)))
      (be `("y" ,y "x" ,lo ,hi) (grid* (lo .to hi) `(,y))))))

(let `((,xl0 ,yl) (,xh0 ,yh)) (bounds<- clay-spots))

;; Extend the x-bounds by 1 each way to allow for flow off the sides.
(let xl xl0.-)
(let xh xh0.+)

(let grid (grid-2d<- (_ xl yl) (_ xh yh) {constant #no}))
(for each! ((p clay-spots))
  (grid .set! (tuple<-list p) #\#))

(to (escaped? (_ _ y))
  (< yh y))

(to (clay? p)
  (= (grid .get p) #\#))

(to (show-map)
  (for each! ((y (spring-y .to< yl)))
    (for each! ((x (xl .to xh)))
      (let p (_ x y))
      (display (if (= spring p) #\+ #\.)))
    (newline))
  (grid .show (on (chars)
                (for each! ((ch chars))
                  (display (or ch #\.)))
                (newline)))
  (newline))

;; (show-map)

;; TODO redo vector+
;;(to (under p)
;;  (vector+ p '(0 1)))
(to (under (_ x y))
  (_ x y.+))

(to (flood)
  (let under-spring (_ spring-x yl))
  (surely (<= spring-y yl))
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
      (let (_ x0 y0) p)
      (to (spilling x dx)
        (let x1 (+ x dx))
        (let next (_ x1 y0))
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
                  (_ x y0)))
      (when (and (every (compose blocked? under) span)  ;; TODO I think this logic is now performed above
                 (blocked? (_ x-min.- y0))
                 (blocked? (_ x-max.+ y0)))
        (for each! ((spot span))
          (surely (= #\| (grid .get spot)) "Converting flowing to still water")
          (grid .set! spot #\~))))))


(do
  (flood)
  (let bag (bag<- grid.values))
  (format "Total water (part 1): ~w\n" (+ (bag #\~) (bag #\|)))
  (format "Still water (part 2): ~w\n" (bag #\~)))
