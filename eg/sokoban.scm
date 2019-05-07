;; Sokoban, ported from github.com/darius/sturm.

;; In this game you live in a 2-d grid world containing crates, target
;; squares, and impassable walls. You need to push all the crates onto
;; targets. You can only push one crate at once. These simple rules
;; yield an elegant game with scope for a great variety of puzzles,
;; from easy to AI-complete.

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

(import (use "lib/sturm")
  cbreak-mode
  get-key render
  green bold unstyled)

(to (main args)
  (let filename
    (match args
      (`(,_) "eg/microban")
      (`(,_ ,fname) fname)
      (_ (error ("Usage: ~d [filename]" .format (args 0))))))
  (call start (with-input-file read-collection filename)))

(to (read-collection source)
  (let name source.read-line)
  (let levels-str source.read-all)
  (let grids (for each ((initial-config (levels-str .split "\n\n")))
               (sokoban-grid<- (parse initial-config))))
  `(,grids ,name))

(to (start grids name)
  (for cbreak-mode ()
    (play grids name 0)))

(let directions (map<- `((#\h  left) (#\j up) (#\k  down) (#\l   right)
                         (left left) (up  up) (down down) (right right))))

;; The UI to a sequence of Sokoban levels.
(to (play grids name level)
  (let trails (array<-list (each flexarray<- grids)))

  (let heading
    "Move with the arrow keys or HJKL. U to undo.
N/P for next/previous level; Q to quit.

Level ~w ~d Move ~w")

  (begin playing ((level level))
    (let trail (trails level))
    (let grid  trail.last)

    (to (view-grid)
      (for each ((ch grid.unparse))
        (let c1 (if ("iI" .find? ch) green unstyled))
        (let c2 (if (".I@" .find? ch) (compose bold c1) c1))
        (c2 ch)))

    (render `(,(heading .format (+ level 1) (name .center 50) trail.count)
              "\n\n"
              ,(view-grid)
              ,@(if grid.won? '("\n\nDone!") '())))

    (let key (get-key))
    (match key.lowercase
      (#\q  'done)
      (#\n  (playing ((+ level 1) .modulo trails.count)))
      (#\p  (playing ((- level 1) .modulo trails.count)))
      (#\u
       (when (< 1 trail.count)
         trail.pop!)
       (playing level))
      (_
       (when (directions .maps? key)
         (let after grid.copy)
         (after .push (directions key))
         (unless (= grid.unparse after.unparse) ;XXX clumsy
           (trail .push! after)))
       (playing level)))))

(to (parse initial-config)
  (surely (do (let lines initial-config.split-lines) ;XXX require
              (let lengths (each '.count lines))  
              (= 1 lengths.range.count)))
  (array<-list initial-config))

(to (sokoban-grid<- grid)
  ;; We represent a grid as a mutable array of characters, including
  ;; the newlines, with every line the same length (which we call the
  ;; width of the grid). Thus moving up or down from some square means a
  ;; displacement by that same width, whatever the starting square.
  (let width (+ (grid .find #\newline) 1))
  (let directions
    (map<- `((left -1) (right 1) (down ,width) (up ,(- width)))))

  (to (find-player)
    (or (grid .find #\i #no)
        (grid .find #\I)))

  ;; Move thing from here to there if possible.
  (to (move! thing here there)
    (when (and (thing .find? (grid here))
               (" ." .find? (grid there)))
      (put! " ." here)
      (put! thing there)))

  ;; Into the square at pos, put thing.
  (to (put! thing pos)
    (let target? (".@I" .find? (grid pos)))
    (grid .set! pos (thing target?.count)))

  (make _
    ;; Try to move the player in the direction.
    ({.push dir}
     (let d (directions dir))
     (let p (find-player))
     (move! "o@" (+ p d) (+ p d d))
     (move! "iI" p (+ p d)))

    ({.won?}
     (not (grid .find? #\o)))

    ({.unparse}
     ((" " .join (each string<- grid)) .replace "\n " "\n"))

    ({.copy}
     (sokoban-grid<- grid.copy))))


(export
  main read-collection
  start play
  sokoban-grid<- parse)
