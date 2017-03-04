;; Sokoban game, ported from github.com/darius/sturm.

;; You move yourself (shown as 'i' or 'I') around a 2-d grid. You can
;; push one crate at a time (each shown as 'o' or '@'). You win when
;; every crate is on a target square: an empty target appears as '.',
;; while one with a crate on it is '@'. (You are shown as 'I' when on a
;; target yourself.) Nothing can move through a wall ('#'). These simple
;; rules yield an elegant game with scope for a tremendous variety of
;; puzzles, from easy to AI-complete.

;; Other console Sokobans display the game a little differently: with
;; different symbols and a squeezed aspect ratio. I insert spaces between
;; squares to make them more nearly, y'know, square.

;; Some other Sokoban implementations you might enjoy:
;; http://eloquentjavascript.net/chapter13.html (by Marijn Haverbeke)
;; http://aurelio.net/projects/sedsokoban/ (by Aurelio Marinho Jargas)
;; http://code.google.com/p/cleese/source/browse/trunk/experimental/necco/kernel/soko.py
;; (runs without a regular OS, by Dave Long)

(import (use "sturm.scm")
  cbreak-mode
  get-key render
  color green compose bold unstyled)

(define (do-command-line argv)
  (let filename
    (match argv.as-list                   ;XXX
      ((_) "sokoban/microban")
      ((_ fname) fname)
      (_ (error "Usage: XXX"))))
  (let (name collection)
    (for with-input-file ((f filename))
      `(,(f.readline .rstrip "\n")    ;XXX
        ,f.read)))                    ;XXX
  (main collection name))

(define (main level-collection name)
  (let grids (each parse (level-collection .split "\n\n")))
  (for cbreak-mode ()
    (play grids name 0)))

;; We represent a grid as a mutable vector of characters, including
;; the newlines, with every line the same length (which we call the
;; width of the grid). Thus moving up or down from some square means a
;; displacement by that same width, whatever the starting square.

(define (parse grid-string)
  (assert (for every ((line grid-string.split-lines))
            (= line.size ((line 0) .size))))
  (vector<-list grid-string))

(define (unparse grid)
  ((" " .join grid) .replace "\n " "\n"))

(define (up    width)  (- width))
(define (down  width)  width)
(define (left  width)  -1)
(define (right width)   1)
(let directions (map<-a-list `((h    ,left) (j    ,down) (k  ,up) (l     ,right)
                               (left ,left) (down ,down) (up ,up) (right ,right))))

;; The UI to a sequence of Sokoban levels.
(define (play grids name level)
  (let trails (vector<-list (for each ((_ grids))
                              (fillvector<-))))
  (let heading "XXX fill in a working format string")

  (begin playing ((level level))
    (let grid  (grids level))
    (let trail (trails level))

    (define (view-grid)
      (for each ((ch (unparse grid)))
        (let c1 (if ("iI" .has? ch) green unstyled)) ;XXX .has?
        (let c2 (if (".I@" .has? ch) (compose bold c1) c1))
        (color c2)))

    (render
     `(,(heading .format (+ level 1) name trail.size)
       ,(view-grid)
       ,@(if (won? grid) '("\n\nDone!") '())))

    (let key ((get-key) .lowercase))
    (match key
      (#\q  'done)
      (#\n  (playing ((+ level 1) .modulo grids.size)))
      (#\p  (playing ((- level 1) .modulo grids.size)))
      (#\u
       (unless trail.empty?
         (grids .set! level trail.pop!))
       (playing level))
      (_
       (when (directions .has? key)
         (let previously grid.copy)
         (push grid (directions key))
         (unless (= (unparse grid) (unparse previously)) ;XXX clumsy
           (trail .push! previously)))
       (playing level)))))

(define (won? grid)
  (not (grid .has? #\o)))

;; Update grid, trying to move the player in the direction.
(define (push grid direction)
  (let i (grid .find (if (grid .has? #\i) #\i #\I))) ;XXX .find
  (let width (+ (grid .find #\newline) 1))
  (let d (direction width))
  (move grid "o@" (+ i d) (+ i d d))
  (move grid "iI" i (+ i d)))

;; Move thing from here to there if possible.
(define (move grid thing here there)
  ;; N.B. `there` is always in bounds when `grid[here] in thing`
  ;; because our grids have '#'-borders, while `thing` is never a '#'.
  (when (and (thing .has? (grid here))
             (" ." .has? (grid there)))
    (lift grid here)
    (drop grid there thing)))

;; Remove any thing (crate or player) from position i.
(define (lift grid i)
  (let dot? (".@I" .has? (grid i)))
  (grid .set! i (" ." (if dot? 1 0))))

;; Into a clear square, put thing.
(define (drop grid i thing)
  (let dot? (= #\. (grid i)))
  (grid .set! i (thing (if dot? 1 0))))
