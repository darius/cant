;; Sokoban, ported from github.com/darius/sturm.

;; In this game you live in a 2-d grid world containing crates, target
;; squares, and impassable walls. You need to push all the crates onto
;; targets. You can only push one crate at once. These simple rules
;; yield an elegant game with scope for a tremendous variety of
;; puzzles, from easy to AI-complete.

;; A square with     is this symbol     or this symbol, when at a target
;; -------------     --------------     --------------------------------
;; Yourself          i                  I
;; A crate           o                  @
;; Nothing                              .
;; A wall            #                  N/A (never a target)

;; Other console Sokobans display the game a little differently: with
;; different symbols and a squeezed aspect ratio. I insert spaces
;; between squares to make them more nearly, y'know, square.

;; Some other Sokoban implementations you might enjoy:
;; http://eloquentjavascript.net/chapter13.html (by Marijn Haverbeke)
;; http://aurelio.net/projects/sedsokoban/ (by Aurelio Marinho Jargas)
;; http://code.google.com/p/cleese/source/browse/trunk/experimental/necco/kernel/soko.py
;; (runs without a regular OS, by Dave Long)

(import (use "sturm")
  cbreak-mode
  get-key render
  color green compose bold unstyled)

(define (do-command-line args)
  (let filename
    (match args
      ((_) "sokoban/microban")
      ((_ fname) fname)
      (_ (error "Usage: %d [filename]" (args 0)))))
  (let (name collection)
    (for with-input-file ((f filename))
      `(,f.read-line ,f.read-all)))
  (main collection name))

(define (main level-collection name)
  (let grids (for each ((initial-config (level-collection .split "\n\n")))
               (sokoban-grid<- (parse initial-config))))
  (for cbreak-mode ()
    (play grids name 0)))

(let directions (map<-a-list
                 `((h    left) (j    down) (k  up) (l     right)
                   (left left) (down down) (up up) (right right))))

;; The UI to a sequence of Sokoban levels.
(define (play grids name level)
  (let trails (vector<-list (for each ((_ grids))
                              (fillvector<-))))
  (let heading
    "Move with the arrow keys or HJKL. U to undo.
N/P for next/previous level; Q to quit.

Level %w %d Move %w")

  (begin playing ((level level))
    (let grid  (grids level))
    (let trail (trails level))

    (define (view-grid)
      (for each ((ch grid.unparse))
        (let c1 (if ("iI" .find? ch) green unstyled))
        (let c2 (if (".I@" .find? ch) (compose bold c1) c1))
        (color c2)))

    (render `(,(heading .format (+ level 1) (name .center 50) trail.count)
              ,(view-grid)
              ,@(if grid.won? '("\n\nDone!") '())))

    (let key ((get-key) .lowercase))
    (match key
      (#\q  'done)
      (#\n  (playing ((+ level 1) .modulo grids.count)))
      (#\p  (playing ((- level 1) .modulo grids.count)))
      (#\u
       (unless trail.empty?
         (grids .set! level trail.pop!))
       (playing level))
      (_
       (when (directions .maps? key)
         (let previously grid.copy)
         (grid .push (directions key))
         (unless (= grid.unparse previously.unparse) ;XXX clumsy
           (trail .push! previously)))
       (playing level)))))

(define (parse initial-config)
  (let lines initial-config.split-lines)
  (assert (for every ((line lines))
            (= line.count ((lines 0) .count))))
  (vector<-list initial-config))

(define (sokoban-grid<- grid)
  ;; We represent a grid as a mutable vector of characters, including
  ;; the newlines, with every line the same length (which we call the
  ;; width of the grid). Thus moving up or down from some square means a
  ;; displacement by that same width, whatever the starting square.
  (let width (+ (grid .find #\newline) 1))
  (let directions
    (map<-a-list `((left -1) (right 1) (down ,width) (up ,(- width)))))

  (define (find-player)
    (or (grid .find #\i #no)
        (grid .find #\I)))

  ;; Move thing from here to there if possible.
  (define (move! thing here there)
    (when (and (thing .find? (grid here))
               (" ." .find? (grid there)))
      (clear! here)
      (drop! thing there)))

  ;; Remove any thing (crate or player) from pos.
  (define (clear! pos)
    (let target? (".@I" .find? (grid pos)))
    (grid .set! pos (if target? #\. #\space)))

  ;; Into a clear square, put thing.
  (define (drop! thing pos)
    (let target? (= #\. (grid pos)))
    (grid .set! pos (thing (if target? 1 0))))

  (make _
    ({.unparse}
     ((" " .join grid) .replace "\n " "\n"))

    ({.copy}
     (sokoban-grid<- grid.copy))

    ({.won?}
     (not (grid .find? #\o)))

    ;; Try to move the player in the direction.
    ({.push dir}
     (let d (directions dir))
     (let p (find-player))
     (move! "o@" (+ p d) (+ p d d))
     (move! "iI" p (+ p d)))))
