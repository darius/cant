(import (use "eg/advent-of-code/utils")
  manhattan-distance<-)

;(let input (with-input-file '.read-lines "eg/advent-of-code/18/data/advent25"))
;(let input (with-input-file '.read-lines "eg/advent-of-code/18/data/advent25.0"))
;(let input (with-input-file '.read-lines "eg/advent-of-code/18/data/advent25.1"))
;(let input (with-input-file '.read-lines "eg/advent-of-code/18/data/advent25.2"))
;(let input (with-input-file '.read-lines "eg/advent-of-code/18/data/advent25.3"))
(let input (with-input-file '.read-lines "eg/advent-of-code/18/data/advent25.4"))

(to (parse line)
  (each number<-string (line.trim .split ","))) ;TODO I thought it was #\,

(let inputs (each parse input))
;;(each! print inputs)

(to (constellate ps)
  (let sets (disjoint-sets<- ps))
  (for each! ((`(,p ,q) (unordered-pairs<- ps)))
;    (print `(,p ,q)))
    (when (<= (manhattan-distance<- p q) 3)
      (sets .union! p q)))
  sets)

(to (disjoint-sets<- elements)
  (let rep (map<- (zip elements elements.keys))) ;; TODO map.inverse method?
  (let next (array<-count elements.count #no))
  (to (chase element)
    (begin chasing ((i (rep element)))
      (match (next i)
        (#no i)
        (j (chasing j)))))
  (make disjoint-sets
    ({.union! x y}
     (let xi (chase x))
     (let yi (chase y))
     ;; TODO path compression, etc.
     (match (xi .compare yi)
       (0)
       (-1 (next .set! yi xi))
       (+1 (next .set! xi yi))))
    ({.count}
     ;; TODO: there's a trickier but more efficient method looking only at the next array, right?
     ('.count (call set<- (each chase elements))))
    ))

(to (unordered-pairs<- ps)
  (case (ps.empty? '())
        (else (let p0 ps.first)
              (chain (for each ((p1 ps.rest))
                       `(,p0 ,p1))
                     (unordered-pairs<- ps.rest)))))


(display "\nPart 1\n")

(to (part-1)
  ((constellate inputs) .count))

(format "~w\n" (part-1))


(display "\nPart 2\n")

(to (part-2)
  'xxx)

(format "~w\n" (part-2))
