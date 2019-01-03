(import (use "eg/advent-of-code/utils")
  simple-parser<-)

(let input (with-input-file '.read-lines "eg/advent-of-code/18/data/advent03"))

(let parse
  (simple-parser<- "'#' :nat ' @ ' :nat ',' :nat ': ' :nat 'x' :nat"))

(let claims (each parse input))


(display "Part 1\n")

(to (area<- `(,_ ,x0 ,y0 ,w ,h))
  (tensor* (x0 .span w) (y0 .span h)))

(let covered-area (call bag<- (gather area<- claims)))

(let n-conflicts (for tally ((n covered-area.values))
                   (< 1 n)))
(print `(the area is ,n-conflicts))


(display "Part 2\n")

(to (undisputed? claim)
  (for every ((point (area<- claim)))
    (= 1 (covered-area point))))

(print `(the undisputed claims are ,(those undisputed? claims)))
