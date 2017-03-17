;; Game of Life

(to (grid<- n-rows n-cols)

  ;; G is an array storing, for each Life grid cell, its value and
  ;; live-neighbor-count. It has two extra rows and columns for 
  ;; the edges. The value is the low bit; the remaining bits hold
  ;; the live neighbor count.
  (let R (+ n-rows 2))
  (let C (+ n-cols 2))
  (let G (vector<-count (* R C) 0))

  (let N (- C))
  (let S C)
  (let W (- 1))
  (let E 1)
  (let NW (+ N W))
  (let NE (+ N E))
  (let SW (+ S W))
  (let SE (+ S E))

  ;; r in [1..n-rows], c in [1..n-cols].
  (to (at r c)
    (+ (* C r) c))

  (to (update i)
    (match (sum (for each ((d `(,NW ,N ,NE
                                 ,W    ,E
                                ,SW ,S ,SE)))
                  (G (+ i d))))
      (2 (G i))
      (3 1)
      (_ 0)))

  (make life-grid
    ((r c)
     (G (at r c)))
    ({.set! r c value}
     (surely ('(0 1) .find? value))
     (G .set! (at r c) value))
    ({.view}
     (for each ((r (range<- 1 (+ n-rows 1))))
       (for each ((c (range<- 1 (+ n-cols 1))))
         (G (at r c)))))
    ({.step}
     ;; TODO: toroidal world
     (let new (grid<- n-rows n-cols))
     (for each ((r (range<- 1 (+ n-rows 1))))
       (for each ((c (range<- 1 (+ n-cols 1))))
         (new .set! r c (update (at r c)))))
     new)
    ))

(to (paint grid top left lines)
  (for each! (((i line) lines.items))
    (for each! (((j ch) line.items))
      (grid .set! (+ top i) (+ left j)
            (if ch.whitespace? 0 1)))))

(to (show grid)
  (for each! ((row grid.view))
    (for each! ((value row))
      (display (" O" value))
      (display " "))
    (newline)))

(to (main _)
  (let grid (grid<- 10 10))
  (paint grid 4 4 '(" **"
                    "** "
                    " * "))
  (begin running ((grid grid) (step 0))
    (show grid)
    (when (< step 10)
      (running grid.step (+ step 1)))))

(export grid<- paint show)
