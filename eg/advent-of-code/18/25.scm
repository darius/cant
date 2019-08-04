;; (Use run.scm to run this.)

(let input (with-input-file _.read-lines data-file))

(to (parse line)
  (each number<-string (line.trim .split ","))) ;TODO I thought it was #\,
;; TODO number<-string should probably allow spaces

(let inputs (each parse input))
;;(each! print inputs)

(to (constellate ps)
  (let sets (partitioning<- ps))
  (for each! ((`(,p ,q) (unordered-pairs<- ps)))
;    (print `(,p ,q)))
    (when (<= (manhattan-distance<- p q) 3)
      (sets .join! p q)))
  sets)

;; Union/find on disjoint sets
;; TODO: even though this worked, it doesn't appear to match the usual
;; algorithm:
;; https://beta.observablehq.com/@bryangingechen/union-find-data-structure
(to (partitioning<- elements)
  (surely (list? elements))
  (let index elements.inverse)
  (let next (array<-count elements.count #no))
  (to (chase element)
    (begin chasing ((i (index element)))
      (be (next i)
        (#no i)
        (j (chasing j)))))
  (make partitioning
    ({.join! x y}
     (let xi (chase x))
     (let yi (chase y))
     ;; TODO path compression, etc.
     (be (xi .compare yi)
       (0)
       (-1 (next .set! yi xi))
       (+1 (next .set! xi yi))))
    ({.count}
     ;; TODO: there's a trickier but more efficient method looking only at the next array, right?
     (let representatives (each chase elements))
     representatives.range.count)
    ))

(to (unordered-pairs<- ps)
  (if ps.empty?
      '()
      (do (let p0 ps.first)
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
