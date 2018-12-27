(import (use "eg/advent-of-code/utils")
  simple-parser<-)

(let input (with-input-file '.read-lines "eg/advent-of-code/18/data/advent10"))

(let parser
  (simple-parser<- "'position=<'_ :int ','_ :int :hug ['> velocity=<'_ :int ','_ :int '>' :hug]"))

(to (parse string)
  ('.results (parser string)))

(let states (each parse input))


(display "\nPart 1\n")

(to (part1)
  (let `(,ps0 ,vs) (transpose states))
  (show ps0)
  (begin stepping ((t 1) (ps ps0))
    (when (= 0 (t .modulo 100))
      (format "time ~w\n" t))
    (let next (zip-with point+ ps vs))
    (when (< 10500 t)
      (format "After ~w seconds:\n" t)
      (if #yes ;(compact? next)
          (show next)
          (format "spread\n")))
    (when (< t 11000)
      (stepping (+ t 1) next))))

(to (point+ `(,x ,y) `(,dx ,dy))
  `(,(+ x dx)
    ,(+ y dy)))
;(to (point+ p q) (zip-with + p q))

(to (compact? ps)
  (let `((,xl ,yl) (,xh ,yh)) (bounds<- ps))
  (let dx (- xh xl))
  (let dy (- yh yl))
  (< dy 50))
;;that could use zip-with too

(to (show points)
  (let `((,xl ,yl) (,xh ,yh)) (bounds<- points))
  (let dx (- xh xl))
  (let dy (- yh yl))
  (format "spread: ~w ~w\n" dx dy)
  (when (< dx 100)
    (let set (call set<- points))
    (for each! ((y (yl .up-to yh)))
      (for each! ((x (xl .up-to xh)))
        (display (if (set `(,x ,y)) #\# #\.)))
      (newline))
    (newline)))

(to (bounds<- points)
  (transpose (each bounds-1d<- (transpose points))))

(to (bounds-1d<- ns)
  `(,(call min ns) ,(call max ns)))

(format "~w\n" (part1))
