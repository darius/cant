;; tic-tac-toe, as a warmup.

(define (tic-tac-toe player opponent grid)
  grid.show
  (newline)
  (case (grid.won?   (format "%d wins.\n" grid.last-to-move))
        (grid.drawn? (format "A draw.\n"))
        (else
         (unless (`(,player ,opponent) .maps-to? human-play)
           (format "%w to move %d. (Press a key.)\n"
                   player grid.whose-move)
;           (get-key)                    ;XXX
           )
         (tic-tac-toe opponent player (player grid)))))

(define (human-play grid)
  "Just ask for a move."
  XXX)

(define (drunk-play grid)
  (arg-min grid.successors drunk-value))

(define (spock-play grid)
  (arg-min grid.successors spock-value))

(define (max-play grid)
  (arg-min grid.successors
           (given (succ) `(,(spock-value succ) ,(drunk-value succ)))))

;; TODO: memoize
(define (drunk-value grid)
  (if grid.won?
      -1
      (match grid.successors
        (() 0)
        (succs (- (average (each drunk-value succs)))))))

(define (spock-value grid)
  (if grid.won?
      -1
      (match grid.successors
        (() 0)
        (succs (- (minimum (each spock-value succs)))))))

(define (average numbers)
  (/ (sum numbers) numbers.count))   ;TODO floats


(define (grid<- p q)

  (define (player-marks)
    (if (= (sum (player-bits p))
           (sum (player-bits q)))
        "XO"
        "OX"))

  (define (player-bits bits)
    (for each ((i (range<- 9)))
      (1 .and (bits .>> i))))

  (make grid
    ({.won?}
     (for some ((way ways-to-win))
       (= way (way .and q))))
    ({.drawn?}
     grid.successors.empty?)
    ({.successors}
     (filter-false                      ;TODO better name
      (for each ((move (range<- 9)))
        (grid .move move))))
    ({.move move}
     (let bit (1 .<< move))
     (and (= 0 (bit .and (p .or q)))
          (grid<- q (p .or bit))))
    ({.whose-move}
     ((player-marks) 0))
    ({.last-to-move}
     ((player-marks) 1))
    ({.show}
     (let marks (player-marks))
     (let values (for each ((pair (zip (player-bits p)
                                       (player-bits q))))
                   (match pair
                     ((1 0) (marks 0))
                     ((0 1) (marks 1))
                     ((0 0) #\.))))
     ;;XXX this writes instead of returning a string
     (call format `(,grid-format ,@(reverse values)))
     (newline))
    ))

(let grid-format ("\n" .join (for each ((_ (range<- 3)))
                               " %d %d %d")))

(let ways-to-win '(0o700 0o070 0o007 0o444 0o222 0o111 0o421 0o124))

(let empty-grid (grid<- 0 0))

(define (move<-human-numbered n)
  (- 9 n))

(define (filter-false xs)
  (filter identity xs))

(define (zip xs ys)
  (match `(,xs ,ys)
    ((() ()) '())
    (((x @xs1) (y @ys1))
     `((,x ,y) ,@(zip xs1 ys1)))))

(define (identity x)
  x)
