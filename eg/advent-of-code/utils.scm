(import (use "lib/parson-core") feed take-1)
(import (use "lib/parson") grammar<- parse)
(let parson-parse parse)

(to (average numbers)
  (surely (not numbers.empty?) "Average of an empty list")
  (/ (sum numbers) numbers.count))

(to (all-mins-by xs fn)
  (for foldl ((best (list<- xs.first))
              (x xs.rest))
    (match ((fn best.first) .compare (fn x))
      (-1 best)
      ( 0 (cons x best))
      (+1 (list<- x)))))
         
(to (fill! array value)                 ;TODO should be a method
  (for each! ((i (range<- array.count)))
    (array .set! i value)))

(to (get/init! map key init)            ;TODO better name
  (unless (map .maps? key)
    (map .set! key (init)))
  (map key))                            ;TODO inefficient

(to (cycle xs)
  (begin cycling ((ys xs))
    (if ys.empty?
        (cycling xs)
        (cons/lazy ys.first (given () (cycling ys.rest))))))

(to (scanl/lazy f z xs)
  (begin scanning ((z z) (xs xs))
    (cons/lazy z
               (given ()
                 (if xs.empty?
                     '()
                     (scanning (f z xs.first) xs.rest))))))

(to (where ok? xs)
  (for filter ((`(,i ,x) xs.items))
    (and (ok? x) i)))

(to (pairs<- xs)
  (begin outer ((outers xs))
    (unless outers.empty?
      (let x1 outers.first)
      (begin inner ((inners outers.rest))
        (if inners.empty?
            (outer outers.rest)
            (do (let x2 inners.first)
                (cons/lazy `(,x1 ,x2)
                           (given () (inner inners.rest)))))))))

(to (filter/lazy f xs)
  (foldr/lazy (given (x z-thunk)
                (let fx (f x))
                (if fx
                    (cons/lazy fx z-thunk)
                    (z-thunk)))
              xs
              (given () '())))

;; prob. too specific, but a lazy duplicates<- might be handier
(to (first-duplicate xs)
  (let seen (set<-))
  (begin looking ((xs xs))
    (let x xs.first)
    (if (seen x)
        x
        (do (seen .add! x)
            (looking xs.rest)))))

(to (deletions s)
  (for each ((i (range<- s.count)))
    `(,(s .slice 0 i)
      ,(s .slice (+ i 1)))))

(to (chain-lines lines)
  (call chain (for each ((line lines))
                (chain line "\n"))))

;; I wish Parson made this convenient without the wrapper:
(to (simple-parser<- template)
  (let grammar (grammar<- (chain "start: " template " :end.\n"
                                 "_ = :whitespace*.")))
  (let peg ((grammar (map<-)) 'start))
  (given (string)
    (parson-parse peg string)))

(to (deduplicate xs)
  ('.keys (call set<- xs)))

(to (product<- xs ys)                     ;TODO generalize
  (for gather ((x xs))
    (for each ((y ys))
      `(,x ,y))))

(to (neighbors<- `(,x ,y))
  (for gather ((dx '(-1 0 1)))
    (for filter ((dy '(-1 0 1)))
      (and (not= `(,dx ,dy) '(0 0))
           `(,(+ x dx) ,(+ y dy))))))

(to (manhattan-distance<- p q)
  (sum (zip-with (compose abs -) p q)))

(export
  get/init! cycle scanl/lazy where pairs<- filter/lazy first-duplicate deletions
  chain-lines all-mins-by average neighbors<-
  simple-parser<- product<- manhattan-distance<-
  grammar<- parson-parse feed take-1)
