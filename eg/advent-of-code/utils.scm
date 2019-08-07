(import (use 'parson-core) feed take-1)

(let parson (use 'parson))
(import parson grammar<-)
(let parson-parse (parson 'parse))

(to (average numbers)
  (surely (not numbers.empty?) "Average of an empty list")
  (/ (sum numbers) numbers.count))

(to (all-mins-by fn xs)
  (for foldl ((best (list<- xs.first))
              (x xs.rest))
    (may ((fn best.first) .compare (fn x))
      (-1 best)
      ( 0 (link x best))
      (+1 (list<- x)))))
         
(to (fill! array value)                 ;TODO should be a mutable-map-trait method
  (for each! ((i array.keys))
    (array .set! i value)))

(to (cycle xs)
  (begin cycling ((ys xs))
    (if ys.empty?
        (cycling xs)
        (link/lazy ys.first (: (cycling ys.rest))))))

(to (scanl/lazy f z xs)
  (begin scanning ((z z) (xs xs))
    (link/lazy z (: (if xs.empty?
                        '()
                        (scanning (f z xs.first) xs.rest))))))

;; TODO is this worth it? sometimes what you want is the yeahs/lazy equivalent
(to (detect include? xs)
  ((those/lazy include? xs) .first))

;; TODO I reimplemented this in 18/25.scm
(to (pairs<- xs)
  (begin outer ((outers xs))
    (unless outers.empty?
      (let x1 outers.first)
      (begin inner ((inners outers.rest))
        (if inners.empty?
            (outer outers.rest)
            (do (let x2 inners.first)
                (link/lazy `(,x1 ,x2)
                           (: (inner inners.rest)))))))))

(to (yeahs/lazy f xs)
  (foldr/lazy (on (x z-thunk)
                (if (let fx (f x))
                    (link/lazy fx z-thunk)
                    (z-thunk)))
              xs
              (: '())))

(to (duplicates<- xs)
  (let seen (set<-))
  (begin looking ((xs xs))
    (if xs.empty?
        '()
        (do (let x xs.first)
            (if (seen .maps? x)
                (link/lazy x (: (looking xs.rest)))
                (do (seen .add! x)
                    (looking xs.rest)))))))

(to (deletions<- s)
  (for each ((i (range<- s.count)))
    `(,(s .slice 0 i)
      ,(s .slice (+ i 1)))))

(to (chain-lines lines)
  (chain @(for each ((line lines))
            (chain line "\n"))))

;; I wish Parson made this convenient without the wrapper:
(to (simple-parser<- template)
  (let grammar (grammar<- (chain "start: " template " :end.\n"
                                 "_ = :whitespace*.")))
  (let peg ((grammar (map<-)) 'start))
  (make parser
    (to (_ string)        ((parson-parse peg string) .results))
    (to (_ .parse string) (parson-parse peg string))))

;; TODO how much slower is this? Doesn't matter since it's no longer used!
;; (to (neighbors<- p)
;;   (for each ((d neighborhood-8))
;;     (vector+ p d))))
(to (neighbors-8<- `(,x ,y))
  (for each ((`(,dx ,dy) neighborhood-8))
    `(,(+ x dx) ,(+ y dy))))

(let neighborhood-8 (for those ((d (grid* '(-1 0 1) '(-1 0 1))))
                      (not= d '(0 0))))

(to (vector+ p q) (each + p q))
(to (vector- p q) (each - p q))

(to (manhattan-distance<- p q)
  (sum (each (compose abs -) p q)))

(to (bounds<- points)
  (transpose (each bounds-1d<- (transpose points))))

(to (bounds-1d<- ns)
  `(,(min @ns) ,(max @ns)))

(export
  cycle scanl/lazy detect pairs<- yeahs/lazy 
  duplicates<- deletions<-
  chain-lines all-mins-by average neighbors-8<-
  simple-parser<- vector+ vector- manhattan-distance<-
  grammar<- parson-parse feed take-1
  bounds<- bounds-1d<- 
  )
