(import (use "advent-utils")
  simple-parser<-)

(let input (with-input-file '.read-lines "eg/advent-of-code/18/data/03.txt"))

(let parser
  (simple-parser<- "'#' :nat ' @ ' :nat ',' :nat ': ' :nat 'x' :nat"))

(to (parse string)
  ('.results (parser string)))

(let claims (each parse input))


(display "Part 1\n")

(to (area<- `(,_ ,x0 ,y0 ,w ,h))
  (for gather ((y (range<- y0 (+ y0 h))))
    (for each ((x (range<- x0 (+ x0 w))))
      `(,x ,y))))

(let covered-area (call bag<- (gather area<- claims)))

(let conflict-area (for those ((n covered-area.values))
                     (< 1 n)))
(print `(the area is ,conflict-area.count))


(display "Part 2\n")

(to (ok? claim)
  (for every ((point (area<- claim)))
    (= 1 (covered-area point))))

(let ok (those ok? claims))
(print `(the unconflicting claims are ,ok))
