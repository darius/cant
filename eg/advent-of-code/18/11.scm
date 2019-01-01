(import (use "eg/advent-of-code/utils")
  product<-)

(let input (with-input-file read "eg/advent-of-code/18/data/advent11"))

(to (level<- x y)
  (let rack-id (+ x 10))
  (- (hundreds-digit (* rack-id (+ (* rack-id y)
                                   grid-serial)))
     5))

(to (hundreds-digit n)
  ((n .quotient 100) .modulo 10))

(let grid-serial input)
(print grid-serial)
;(print (level<- 101 153))

(display "\nPart 1\n")

(to (part1)
  (max-by patch-level<- (patches)))

(to (patch-level<- `(,x ,y))
  (sum (for each ((`(,x ,y) (product<- (x .span 3)
                                       (y .span 3))))
         (level<- x y))))

(to (patches)
  (product<- (1 .to 298)
             (1 .to 298)))

(format "~w\n" (part1))


(display "\nPart 2\n")

(let N 300)

(to (part2)
  ;; I'm gonna use 0-based coords and then convert up at the end.
  (let N1 (+ N 1))
  (to (at x y)
    (+ (* y N1) x))

  (let S (array<-count (* N1 N1) 0))
  ;; (S (at x y)) = sum of (A i j) for i,j above and left of x,y.

  (for each! ((y (1 .to N)))
    (for each! ((x (1 .to N)))
      (S .set! (at x y)
         (+ (S (at x (- y 1)))          ; rect above
            (- (S (at (- x 1) y))       ; strip to left
               (S (at (- x 1) (- y 1))))
            (level<- x y)))))           ; this spot
  (format "S initialized\n")

  (to (power<- `(,x0 ,y0 ,d))
    (let x1 (+ x0 d))
    (let y1 (+ y0 d))
    (+ (S (at x1 y1))
       (- (S (at x0 y1)))
       (- (S (at x1 y0)))
       (S (at x0 y0))))

  (let side (0 .to< N))

  (to (subsquares)
    (for gather ((i side))
      (for gather ((j side))
        (for each ((d (1 .to (min (- N i) (- N j)))))
          `(,i ,j ,d)))))

  (to (cvt-coords `(,x ,y ,d))
    `(,(+ x 1) ,(+ y 1) ,d))

;  (cvt-coords (max-by power<- (subsquares))))
; out of memory

  (to (corners)
    (product<- side side))

  (let `(,power ,triple)
    (call max
          (for each ((`(,x ,y) (corners)))
            (call max
                  (for each ((d (1 .to (min (- N x) (- N y)))))
                    (let triple `(,x ,y ,d))
                    `(,(power<- triple) ,triple))))))
                  
  (cvt-coords triple))

(let `(,x ,y ,s) (part2))
(format "~w,~w,~w\n" x y s)
