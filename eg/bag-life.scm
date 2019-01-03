;; Game of Life again.
;; Represent a grid as a set of populated locations.
;; A location is an `(,x ,y) coordinate pair.

(import (use "lib/sturm")
  cbreak-mode)
(import (use "lib/ansi-term")
  home clear-screen cursor-show cursor-hide)

(to (main `(,prog ,@args))
  (let n-steps (match args
                 ('() 20)
                 (`(,n-str) (number<-string n-str))
                 (_ (error ("Usage: ~d [#steps]" .format prog)))))
  (let grid r-pentomino)
  (for cbreak-mode ()
    (display cursor-hide)
    (begin running ((grid grid) (step 0))
      (display clear-screen)
      (display home)
      (show grid)
      (when (< step n-steps)
        (running (update grid) (+ step 1))))
    (display cursor-show)))

(to (update grid)
  (let active (bag<- (gather neighbors grid.keys)))
  ('.range (for filter ((`(,pos ,n-live) active.items))
             (match n-live
               (3 pos)
               (2 (and (grid .maps? pos) pos))
               (_ #no)))))

(to (neighbors `(,x ,y))
  (for gather ((dx '(-1 0 1)))
    (for filter ((dy '(-1 0 1)))
      (and (not= `(,dx ,dy) '(0 0))
           `(,(+ x dx) ,(+ y dy))))))

(to (show grid)
  (let `((,x-lo ,x-hi) (,y-lo ,y-hi))
    (each bounds<- (transpose grid.keys)))
  (for each! ((y (y-lo .to y-hi)))
    (for each! ((x (x-lo .to x-hi)))
      (display (if (grid .maps? `(,x ,y)) "O " "  ")))
    (newline)))

(to (bounds<- numbers)
  `(,(call min numbers)
    ,(call max numbers)))

(to (paint lines)              ;TODO maybe use a `where` function
  ('.range (for gather ((`(,row ,line) lines.items))
             (for filter ((`(,col ,ch) line.items))
               (and (not ch.whitespace?)
                    `(,row ,col))))))

(let r-pentomino (paint '(" **"
                          "** "
                          " * ")))

(export update show paint r-pentomino)
