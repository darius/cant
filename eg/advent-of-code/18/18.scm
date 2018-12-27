(import (use "lib/grid-2d")
  grid-2d<-)

(let input (with-input-file '.read-lines "eg/advent-of-code/18/data/advent18"))

(let width input.first.count)
(let height input.count)
(let bottom-right `(,(- width 1) ,(- height 1)))

(let grid (grid-2d<- '(0 0)
                     bottom-right
                     {map (given (`(,x ,y)) ((input y) x))}))

(to (show grid)
  (grid .show (given (row)
                (each! display row)
                (newline)))
  (newline))

(show grid)
;; open ground (.), trees (|), or a lumberyard (#)


(display "\nPart 1\n")

(to (part-1)
  (begin stepping ((t 0) (grid grid))
    (show grid)
    (if (= t 10)
        (tally grid)
        (stepping (+ t 1) (step grid)))))

(to (step grid)
  (let new (grid-2d<- '(0 0)
                      bottom-right
                      {map (update grid)})))
  
(to (tally grid)
  (let bag (call bag<- grid.values))
  (* (bag #\#) (bag #\|)))

(to ((update grid) p)
  (let wood (box<- 0))
  (let lumber (box<- 0))
  (to (tally x1 y1)
    (match (grid .get `(,x1 ,y1))
      (#\.)
      (#\| (wood .^= (+ wood.^ 1)))
      (#\# (lumber .^= (+ lumber.^ 1)))
      (_)))
  (let `(,x ,y) p)
  (tally (- x 1) (- y 1))
  (tally x       (- y 1))
  (tally (+ x 1) (- y 1))
  (tally (- x 1) y)
  ;; leaving out (tally x y)
  (tally (+ x 1) y)
  (tally (- x 1) (+ y 1))
  (tally x       (+ y 1))
  (tally (+ x 1) (+ y 1))

  (match (grid p)
    (#\. (if (<= 3 wood.^) #\| #\.))
    (#\| (if (<= 3 lumber.^) #\# #\|))
    (#\# (if (and (<= 1 lumber.^)
                  (<= 1 wood.^))
             #\#
             #\.))))

;;(format "~w\n" (part-1))


(display "\nPart 2\n")

(to (part-2)
  (begin stepping ((t 0) (grid grid))
    (let bag (call bag<- grid.values))
    (format "After ~w minutes: ~w trees, ~w lumber\n" t (bag .get #\|) (bag .get #\#))
    (stepping (+ t 1) (step grid))))

(format "~w\n" (part-2))
