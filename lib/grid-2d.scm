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

  (let A (array<-count (* x-extent y-extent)
                       (be initializer
                         ({constant value} value)
                         ({map _} #no))))
  (let at
    (if (and (= xl 0) (= yl 0))
        (on (x y)                    ;special-cased for speed
          (+ (* y x-extent) x))
        (on (x y)        ;maybe a hashmap is actually more efficient?
          (+ (* (- y yl) x-extent)
             (- x xl)))))

  (to (check x y)
    (unless (<= xl x xh) (error "x coordinate out of range" x))
    (unless (<= yl y yh) (error "y coordinate out of range" y)))

  (be initializer
    ({constant _})
    ({map f}
     (for each! ((y (yl .to yh)))
       (for each! ((x (xl .to xh)))
         (A .set! (at x y) (f `(,x ,y)))))))

  (make grid-2d {extending map-trait}
    
    (to (_ `(,x ,y))
      (check x y)
      (A (at x y)))
    (to (_ .set! `(,x ,y) value)
      (check x y)
      (A .set! (at x y) value))
    (to (_ .get `(,x ,y) default)
      (if (and (<= xl x xh)
               (<= yl y yh))
          (A (at x y))
          default))
    (to (_ .maps? `(,x ,y))
      (and (<= xl x xh)
           (<= yl y yh)))

    (to _.count
      A.count)

    (to _.items
      (zip grid-2d.keys A.values))
    (to _.keys
      ;; Could be a one-liner: (grid* (xl .to xh) (yl .to yh))
      ;; except that'd be column-major order. Hm, hm.
      (for gather ((y (yl .to yh)))
        (for each ((x (xl .to xh)))
          `(,x ,y))))
    (to _.values
      A.values)

    (to (_ .show print-line!)
      (for each! ((y (yl .to yh)))
        (print-line! (for each ((x (xl .to xh)))
                       (A (at x y))))))
    (to _.show
      (grid-2d .show print))
    (to (_ .selfie sink)
      (sink .display ("#<grid-2d (~w,~w)..(~w,~w)>"
                      .format xl yl xh yh)))
    ))

(export grid-2d<-)

;; -> (let g (grid-2d<- '(2 1) '(4 2) '*))
;; #<grid-2d (2,1)..(4,2)>
;; -> (g 2 2)
;; ...
;; Match failure: (#<grid-2d (2,1)..(4,2)> (2 2))
;; -> (g '(2 2))
;; *
;; -> (g .set! '(2 2) 'a)
;; -> g
;; #<grid-2d (2,1)..(4,2)>
;; -> g.show
;; (* * *)
;; (a * *)
;; #no
;; -> (g .show (on (row) (format "~d\n" ("" .join (each _.name row)))))
;; ***
;; a**
;; #no
;; -> (g .set! '(3 1) 'b)
;; -> (g .show (on (row) (format "~d\n" ("" .join (each _.name row)))))
;; *b*
;; a**
;; #no
;; -> (g .get '(3 1))
;; b
;; -> (g .get '(3 3))
;; #no
;; -> (g .get '(3 3) 'blah)
;; blah
;; -> (g '(2 42))
;; ...
;; y coordinate out of range: (42)
