;; Ported from github.com/darius/sturm

(import (use 'random)
  rng<- random-seed<-)
(import (use 'sturm)
  cbreak-mode get-key render
  ;;XXX import is finally a pain:
  bold underlined blinking inverted unstyled
  black red green yellow blue magenta cyan white
  on-black on-red on-green on-yellow on-blue on-magenta on-cyan on-white)

(to (main _)
  (let rng (rng<- (random-seed<-)))
  (for cbreak-mode ()
    (play (starting-board<- rng) rng)))

(to (play board rng)
  (let history (flexarray<-))
  (begin playing ((board board) (forfeit? #no))
    (to (continue)
      (playing board forfeit?))
    (let score (hm (if (lost? board) "You lose!")
                   (if forfeit?      "You forfeit.")
                   (if (won? board)  "You win!")
                   (else             "")))
    (frame board score)
    (may get-key.lowercase
      (be #\q 'quitting)
      (be #\u (if history.empty?
                  (continue)
                  (playing history.pop! #yes)))
      (be key (may (arrows .get key)
                (be #no (continue))
                (be direction
                  (may (direction board)
                    (be '() (continue))
                    (be sliding
                      (history .push! board)
                      (animate sliding score)
                      (let small? (< (rng .random-integer 10) 9))
                      (let next-board (plop rng sliding.last (if small? 2 4)))
                      (playing next-board forfeit?)))))))))

(let heading "Use the arrow keys, U to undo (and forfeit), or Q to quit.\n\n")

(to (frame board score)
  (render [heading (view board) score]))

(to (animate boards score)
  (for each! ((board boards))
    (frame board score)
    (nanosleep 40000000)))

(to (starting-board<- rng)
  (plop rng (plop rng empty-board 2) 2))

(to (plop rng board value)
  (update board (random-empty-square rng board) value))

(to (view rows)
  (for each ((row rows))
    [(for each ((v row))
       [" " (or (tiles .get v)
                (bold ("~w" .format v)))])
     "\n\n"]))

(let tiles
  (map<- `((   0                                      "  . ")
           (   2 ,(on-blue (white                     "  2 ")))
           (   4 ,(on-red (black                      "  4 ")))
           (   8 ,(white (on-magenta                  "  8 ")))
           (  16 ,(black (on-cyan                     " 16 ")))
           (  32 ,(black (on-green                    " 32 ")))
           (  64 ,(black (on-yellow                   " 64 ")))
           ( 128 ,(black (on-white                    "128 ")))
           ( 256 ,(bold (blue (on-black               "256 "))))
           ( 512 ,(bold (magenta (on-black            "512 "))))
           (1024 ,(underlined (bold (red (on-black    "1024")))))
           (2048 ,(underlined (bold (yellow (on-black "2048"))))))))

(to (won? rows)
  (for some ((row rows))
    (for some ((v row))
      (<= 2048 v))))

(to (lost? rows)
  (for every ((move arrows.values))
    ((move rows) .empty?)))

(to (random-empty-square rng rows)
  (rng .pick (for gather ((`(,r ,row) rows.items))
               (for yeahs ((`(,c ,v) row.items))
                 (and (= v 0) `(,r ,c))))))

(to (update rows at new-value)
  (for each ((`(,r ,row) rows.items))
    (for each ((`(,c ,v) row.items))
      (if (= at `(,r ,c)) new-value v))))

;; Try to slide the board leftward; return a list of boards to
;; animate the move -- an empty list if there's no move leftward.
(to (left rows)
  (begin sliding ((states (for each ((row rows))
                            (slide `(0 ,row)))))
    (let `(,lows ,board) (transpose states))
    (if (for every ((lo lows))
          (<= 4 lo))
        '()
        `(,board ,@(sliding (each slide states))))))

;; Slide row one place leftward, leaving fixed any places left of the
;; position at `low`. Advance low past merging or completion. Return
;; the updated `(,low ,row) state.
(to (slide `(,low ,row))
  (begin checking ((i low))
    (let j i.+)
    (hm (when (<= 4 j)
          `(4 ,row)) ; There was no space or coincidence to slide into.
        (do (let same? (= (row i) (row j))))
        (when (= same? (= (row i) 0))
          (checking j))
        (else ; Found one, let's slide:
          (let sum (+ (row i) (row j)))
          (let slid
            `(,@(row .slice 0 i) ,sum ,@(row .slice j.+) 0))
          `(,(if same? j low) ,slid)))))

(to (right rows) (each flip-h (left (flip-h rows))))
(to (up rows)    (each flip-d (left (flip-d rows))))
(to (down rows)  (each flip-d (right (flip-d rows))))

(to (flip-h rows)                       ; horizontal flip
  (each reverse rows))

(to (flip-d rows)                       ; diagonal flip
  (transpose rows))

(let arrows (export left right up down))

(let empty-board ('((0 0 0 0)) .repeat 4))

(export 
  main
  play starting-board<- lost? won? left right up down)
