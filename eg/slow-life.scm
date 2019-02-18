;; Game of Life

(import (use "lib/sturm")
  cbreak-mode)
(import (use "lib/ansi-term")
  home clear-screen cursor-show cursor-hide)

(to (main `(,prog ,@args))
  (let n-steps (match args
                 ('() 20)
                 (`(,n-str) (number<-string n-str))
                 (_ (error ("Usage: ~d [#steps]" .format prog)))))
  (let grid (grid<- 24 39))
  (paint grid 10 18 '(" **"             ;TODO: read in a pattern
                      "** "
                      " * "))
  (for cbreak-mode ()
    (display cursor-hide)
    (begin running ((grid grid) (step 0))
      (display clear-screen)
      (display home)
      grid.show
      (when (< step n-steps)
        (running grid.next (+ step 1))))
    (display cursor-show)))

(to (smoke-test)
  (let grid (grid<- 8 8))
  (paint grid 3 3 '(" **"
                    "** "
                    " * "))
  grid.show
  (let gen1 grid.next)
  gen1.show
  gen1.next.show)

(to (paint grid top left lines)
  (let bottom (+ top lines.count -1))
  (for each! ((`(,i ,line) lines.items))
    (for each! ((`(,j ,ch) line.items))
      (grid .set! (- bottom i) (+ left j)
            (if ch.whitespace? 0 1)))))

(to (grid<- n-rows n-cols)

  ;; G is an array storing, for each Life grid cell, its value (0 or
  ;; 1). It has two extra rows and columns for the edges.
  (let R (+ n-rows 2))
  (let C (+ n-cols 2))
  (let G (array<-count (* R C) 0))

  (let N (- C))
  (let S C)
  (let W (- 1))
  (let E 1)
  (let neighbor-dirs
    `(,(+ N W) ,N ,(+ N E)
      ,W          ,E
      ,(+ S W) ,S ,(+ S E)))

  ;; r in [1..n-rows], c in [1..n-cols].
  (to (at r c)
    (+ (* C r) c))

  (to (update i)
    (match (sum (for each ((dir neighbor-dirs))
                  (G (+ i dir))))
      (2 (G i))
      (3 1)
      (_ 0)))

  ;; Make the world toroidal by copying the edges of the array so that
  ;; row #1 effectively neighbors row #n-rows, and likewise for the
  ;; columns.
  (to (copy-edges)
    (for each! ((r (1 .to n-rows)))
      (G .set! (at r 0)       (G (at r n-cols)))
      (G .set! (at r (- C 1)) (G (at r 1))))
    (copy! (at 0 0)       (at n-rows 0) C)
    (copy! (at (- R 1) 0) (at 1 0)      C))

  (to (copy! dest lo n)  ;TODO use array-trait .move!
   (for each! ((i (range<- n)))
     (G .set! (+ dest i) (G (+ lo i)))))

  (make life-grid
    (`(,r ,c)
     (G (at r c)))
    ({.set! r c value}
     (surely ('(0 1) .find? value))
     (G .set! (at r c) value))
    ({.show}
     (for each! ((r (1 .to n-rows)))
       (for each! ((c (1 .to n-cols)))
         (display (" O" (G (at r c))))
         (display " "))
       (newline)))
    ({.next}
     (copy-edges)
     (let new (grid<- n-rows n-cols))
     (for each! ((r (1 .to n-rows)))
       (for each! ((c (1 .to n-cols)))
         (new .set! r c (update (at r c)))))
     new)
    ))

(export grid<- paint smoke-test)
