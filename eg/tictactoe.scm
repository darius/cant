;; tic-tac-toe, as a warmup.

(import (use "lib/memoize.scm") memoize)

(define (tic-tac-toe player opponent grid)
  (show grid)
  (newline)
  (case ((won? grid)   (format "%d wins.\n" (last-to-move grid)))
        ((drawn? grid) (format "A draw.\n"))
        (else
         (unless (`(,player ,opponent) .maps-to? human-play)
           (format "%w to move %d. (Press a key.)\n"
                   player (whose-move grid))
;           (get-key)                    ;XXX
           )
         (tic-tac-toe opponent player (player grid)))))

(define (human-play grid)
  "Just ask for a move."
  XXX)

(define (drunk-play grid)
  (arg-min (successors grid) drunk-value))

(define (spock-play grid)
  (arg-min (successors grid) spock-value))

(define (max-play grid)
  (arg-min (successors grid)
           (given (succ) `(,(spock-value succ) ,(drunk-value succ)))))

(let drunk-value
  (memoize (given (grid)
             (if (won? grid)
                 -1
                 (match (successors grid)
                   (() 0)
                   (succs (- (average (each drunk-value succs)))))))))

(let spock-value
  (memoize (given (grid)
             (if (won? grid)
                 -1
                 (match (successors grid)
                   (() 0)
                   (succs (- (call min (each spock-value succs)))))))))

(define (average numbers)
  (/ (sum numbers) numbers.count))


(define (player-marks {grid p q})
  (if (= (sum (player-bits p))
         (sum (player-bits q)))
      "XO"
      "OX"))

(define (player-bits bits)
  (for each ((i (range<- 9)))
    (1 .and (bits .>> i))))

(define (won? {grid p q})
  (for some ((way ways-to-win))
    (= way (way .and q))))

(define (drawn? grid)
  ((successors grid) .empty?))

(define (successors grid)
  (filter-false                      ;TODO better name
   (for each ((move (range<- 9)))
     (apply-move grid move))))

(define (apply-move {grid p q} move)
  (let bit (1 .<< move))
  (and (= 0 (bit .and (p .or q)))
       {grid q (p .or bit)}))

(define (whose-move grid)
  ((player-marks grid) 0))

(define (last-to-move grid)
  ((player-marks grid) 1))

(define (show {grid p q})
   (let marks (player-marks {grid p q}))
   (let values (for each ((pair (zip (player-bits p)
                                     (player-bits q))))
                 (match pair
                   ((1 0) (marks 0))
                   ((0 1) (marks 1))
                   ((0 0) #\.))))
   (call format `(,grid-format ,@(reverse values)))
   (newline))

(let grid-format ("\n" .join (for each ((_ (range<- 3)))
                               " %d %d %d")))

(let ways-to-win '(0o700 0o070 0o007 0o444 0o222 0o111 0o421 0o124))

(let empty-grid {grid 0 0})

(define (move<-human-numbered n)
  (- 9 n))

(define (filter-false xs)
  (filter identity xs))


(hide
 (let g {grid 0o610 0o061})
 (tic-tac-toe spock-play spock-play g)
)
