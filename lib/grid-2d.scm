;; Dense 2-d arrays with arbitrary bounds

;; TODO should indices be like (row,col) or (x,y)? It sucks that
;; there's a choice.

;; TODO the input ranges are inclusive. Is that the Right Thing?
;; Probably not.

;; TODO make this go together somehow with lib/format-tables

;; TODO design a tensor abstraction; this would be a specialized
;; implementation, maybe? Or would we prefer anchoring tensor ranges
;; at 0?

;; TODO APL/numpy-style ops

(to (grid-2d<- `(,xl ,yl) `(,xh ,yh) initializer)
  (let x-extent (+ (- xh xl) 1))
  (let y-extent (+ (- yh yl) 1))
  (surely (<= 0 x-extent))
  (surely (<= 0 y-extent))

  (let N (* x-extent y-extent))
  (let A (array<-count N (match initializer
                           ({constant value} value)
                           ({map _} #no))))
  (let at
    (if (and (= xl 0) (= yl 0))
        (given (x y)                    ;special-cased for speed
          (+ (* y x-extent) x))
        (given (x y)        ;maybe a hashmap is actually more efficient?
          (+ (* (- y yl) x-extent)
             (- x xl)))))

  (to (check x y)
    (unless (<= xl x xh) (error "x coordinate out of range" x))
    (unless (<= yl y yh) (error "y coordinate out of range" y)))

  (match initializer
    ({constant _})
    ({map f}
     (for each! ((y (yl .up-to yh)))
       (for each! ((x (xl .up-to xh)))
         (A .set! (at x y) (f `(,x ,y)))))))

  (make grid-2d {extending map-trait}  ; TODO check OK with map-trait 
    
    (`((,x ,y))
     (check x y)
     (A (at x y)))
    ({.set! `(,x ,y) value}
     (check x y)
     (A .set! (at x y) value))
    ({.get `(,x ,y) default}
     (if (and (<= xl x xh)
              (<= yl y yh))
         (A (at x y))
         default))
    ({.get pos}
     (grid-2d .get pos #no))
    ({.maps? `(,x ,y)}
     (and (<= xl x xh)
          (<= yl y yh)))

    ({.keys}
     (for gather ((y (yl .up-to yh)))   ;TODO tensor-product
       (for each ((x (xl .up-to xh)))
         `(,x ,y))))
    ({.values}
     A.values)

    ({.show print-line!}
     (for each! ((y (yl .up-to yh)))
       (print-line! (for each ((x (xl .up-to xh)))
                      (A (at x y))))))
    ({.show}
     (grid-2d .show print))
    ({.selfie sink}
     (sink .display ("#<grid-2d (~w,~w)..(~w,~w)>"
                     .format xl yl xh yh)))
    ))

(export grid-2d<-)

;; sqm> (let g (grid-2d<- '(2 1) '(4 2) '*))
;; #<grid-2d (2,1)..(4,2)>
;; sqm> (g 2 2)
;; ...
;; Match failure: (#<grid-2d (2,1)..(4,2)> (2 2))
;; sqm> (g '(2 2))
;; *
;; sqm> (g .set! '(2 2) 'a)
;; sqm> g
;; #<grid-2d (2,1)..(4,2)>
;; sqm> g.show
;; (* * *)
;; (a * *)
;; #no
;; sqm> (g .show (given (row) (format "~d\n" ("" .join (each '.name row)))))
;; ***
;; a**
;; #no
;; sqm> (g .set! '(3 1) 'b)
;; sqm> (g .show (given (row) (format "~d\n" ("" .join (each '.name row)))))
;; *b*
;; a**
;; #no
;; sqm> (g .get '(3 1))
;; b
;; sqm> (g .get '(3 3))
;; #no
;; sqm> (g .get '(3 3) 'blah)
;; blah
;; sqm> (g '(2 42))
;; ...
;; y coordinate out of range: (42)
