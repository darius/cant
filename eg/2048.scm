;; Ported from github.com/darius/sturm

(import (use "lib/sturm")
  cbreak-mode
  get-key
  render)

(to (main _)
  (for cbreak-mode ()
    (play (starting-board<-))))

(to (play board)
  (let history (fillvector<-))
  (begin playing ((board board) (forfeit? #no))
    (to (continue)
      (playing board forfeit?))
    (let score (case ((lost? board) "You lose!")
                     (forfeit?      "You forfeit.")
                     ((won? board)  "You win!")
                     (else          "")))
    (frame board score)
    (let key (get-key))
    (case (("Qq" .find? key) 'quitting)
          (("Uu" .find? key)
           (if history.empty?
               (continue)
               (playing history.pop! #yes)))
          ((arrows .maps? key)
           (let sliding ((arrows key) board))
           (case (sliding.empty? (continue))
                 (else
                  (history .push! board)
                  (animate sliding score)
                  (let next-board (plop sliding.last
                                        (if (< (random-integer 10) 9) 2 4)))
                  (playing next-board forfeit?))))
          (else
           (continue)))))

(let heading "Use the arrow keys, U to undo (and forfeit), or Q to quit.\n\n")

(to (frame board score)
  (render `(,heading ,(view board) ,score)))

(to (animate boards score)
  (for each! ((board boards))
    (frame board score)
    (nanosleep 40000000)))

(to (starting-board<-)
  (plop (plop empty-board 2) 2))

(to (plop board value)
  (update board (random-empty-square board) value))

;; XXX this object doesn't really buy us anything
;; we could just use the list this time


(to (view rows)
  (for each ((row rows))
    `(,(for each ((v row))
         (let s (if (= v 0) "." ("~w" .format v)))
         (" ~d" .format (s .center 4)))
      "\n\n")))

(to (won? rows)
  (for some ((row rows))
    (for some ((v row))
      (<= 2048 v))))

(to (lost? rows)
  (for every ((move arrows.values))
    ((move rows) .empty?)))

(to (random-empty-square rows)
  (random-choice (for gather (((r row) rows.items))
                   (for filter (((c v) row.items))
                     (and (= v 0) `(,r ,c))))))

(to (random-choice xs)       ;XXX should be in lib or standard methods
  (xs (random-integer xs.count)))

(to (update rows at new-value)
  (for each (((r row) rows.items))
    (for each (((c v) row.items))
      (if (= at `(,r ,c)) new-value v))))

;; Try to slide the board leftward; return a list of boards to
;; animate the move -- an empty list if there's no move leftward.
(to (left rows)
  ;; TODO could probably be simpler
  (begin sliding ((states (for each ((row rows))
                            (slide 0 row))))
    (if (for every (((lo _) states))
          (<= 4 lo))
        '()
        (cons (for each (((_ row) states))
                row)
              (sliding (for each (((lo row) states))
                         (slide lo row)))))))

;; Slide row one place leftward, leaving fixed any places left of lo.
;; Advance lo past merging or completion.
(to (slide lo row)
  ;; TODO could probably be clearer too
  (begin checking ((i (+ lo 1)))
    (if (<= 4 i)
        `(4 ,row)
        (do (let same? (= (row (- i 1)) (row i)))
            (if (not= same? (= (row (- i 1)) 0))
                `(,(if same? i lo)
                  (,@(row .slice 0 (- i 1))
                   ,(+ (row (- i 1)) (row i))
                   ,@(row .slice (+ i 1))
                   0))
                (checking (+ i 1)))))))

(to (right rows) (each flip-h (left (flip-h rows))))
(to (up rows)    (each flip-d (left (flip-d rows))))
(to (down rows)  (each flip-d (right (flip-d rows))))

(to (flip-h rows)                       ; horizontal flip
  (each reverse rows))

(to (flip-d rows)                       ; diagonal flip
  (transpose rows))

;; TODO: name it (zip @rows) instead, like Python?
(to (transpose rows)
  (if (every '.empty? rows)   ; and make it (some '.empty? rows)?
      '()
      `(,(each '.first rows)
        ,@(transpose (each '.rest rows)))))

(let arrows (export left right up down))

(let empty-board '((0 0 0 0)   ;XXX crude
                   (0 0 0 0)
                   (0 0 0 0)
                   (0 0 0 0)))

(export 
  main
  play starting-board<- lost? won? left right up down)
