;; Unfinished port of one-day-i-will-play-chess

(to ((main<- strategy-names) args)
  (display "(Moves look like 'e2e3')\n\n")
  (play-chess (strategy-names (args 1))
              (strategy-names (args 2))))

(to (play-chess white-strategy black-strategy)
  (play (initial-chess-board<-) `(,white-strategy ,black-strategy)))

(to (play board players)
  (begin playing ((board board))
    (case (board.outcome
           (print board))
          (else
           (print board)
           (display "\n\n")
           (playing (board .play-turn players))))))

(to (human-player board)
  (begin asking ()
    (display ("~d, your move? " .format board.mover.name.capitalize))
    (match stdin.read-line  ;TODO extract the stdin cap
      ((? eof?) {resign})
      (answer (or (board .parse-move answer)
                  (do (display "Illegal move\n")
                      (asking)))))))

(to (random-player<- rng)
  (to (random-player board)
    (rng .choice (as-list board.gen-legal-moves))))

(to (greedy-player board)
  (for min-by ((move board.gen-legal-moves))
    (greedy-evaluate (update move board))))

(to (minimax-player<- depth)
  (to (player board)
    (for min-by ((move board.gen-legal-moves))
      (minimax-evaluate (update move board) depth))))

;; Return the mover's weighted piece advantage.
(to (greedy-evaluate board)
  (let total (sum (each piece-values board.squares)))
  ;; Just a little randomness makes play less boring:
  ;;XXX need to set up with an rng
  ;; total += random.uniform(0, 0.001)
  (match board.mover
    ('white (- total))
    (_      total)))

(to (minimax-evaluate board depth)
  (match depth
    (0 (greedy-evaluate board))
    (_ (match board.get-piece-moves
         ('() 0)
         (moves (- (min (for each ((move moves))
                          (minimax-evaluate (update move board) (- depth 1))))))))))

(let piece-values
  (map<- '((#\p 10)
           (#\n 31)
           (#\b 33)
           (#\r 50)
           (#\q 90)
           (#\k 10000))))
(for each! ((`(,p ,v) piece-values.items))
  (piece-values .set! p.uppercase (- v)))
(piece-values .set! #\space 0)
(piece-values .set! #\-     0)

(to (initial-chess-board<-)
  ;; XXX ok, what rep is nicest here?
  (let squares '("----------"           ;TODO a vector instead?
                 "-rnbqkbnr-"
                 "-pppppppp-"
                 "-        -"
                 "-        -"
                 "-        -"
                 "-        -"
                 "-PPPPPPPP-"
                 "-RNBQKBNR-"
                 "----------"))
  (let castling '((#yes #yes) (#yes #yes))) ;TODO vector instead?
  (board<- 'white squares castling #no #no))

(to (board<- mover squares castling en-passant-target outcome-param)
  (make board

    ({.mover} mover)

    ({.selfie sink}
     (let lines (for each ((line (squares .slice 1 9)))
                  (line .slice 1 9)))
     (for each! ((`(,i ,line) lines.items))
       (format .to-sink sink "~w|" (- 8 i))
       (for each! ((`(,j ,piece) line.items))
         (format .to-sink sink " ~d" (if (and (= piece #\space)
                                              ((+ i j) .odd?))
                                         #\. ; An empty black square.
                                         piece)))
       (format .to-sink sink "\n"))
     (format .to-sink sink " +----------------\n")
     (format .to-sink sink "   a b c d e f g h    ~d\n"
             (match board.outcome
               (#no   ("~w to move" .format mover))
               ('draw "A draw.")
               (w     ("~d wins!" .format w.name.capitalize)))))

    ;; Return #no, draw, black, or white (meaning the winner).
    ({.outcome}
     (or outcome-param
         (case (board.checkmate?             (opponent mover))
               (board.get-piece-moves.empty? 'draw)
               (else                         #no))))

    ;; Is the player to move checkmated?
    ({.checkmate?}  ;;XXX are we getting this at the start? why?
     ;; Or: is the mover in check now, and in check after every possible move?
     ;; XXX also seems buggy in other ways, though these boards are hard to read in the console
     (to (in-check?)                    ;XXX redundant and untested. Not yet in the Python version.
       (let my-king (match mover
                      ('white #\K)
                      ('black #\k)))
       (let swap-players (board<- (opponent mover)
                                  squares castling en-passant-target outcome-param))
       (not (for every ((succ swap-players.gen-successors))
              (succ.squares .find? my-king))))
     (and (in-check?)
          (every '.checking? board.gen-successors)))

    ;; Is the opponent in check?
    ({.checking?}
     ;; Operationalized as: can the mover take the opposing king?
     (let opposing-king (match mover
                          ('white #\k)
                          ('black #\K)))
     (not (for every ((succ board.gen-successors))
            (succ.squares .find? opposing-king))))

    ;; The boards that can result from a move (other than resigning).
    ({.gen-successors}
     (for each ((move board.get-piece-moves))
       (update move board)))

    ({.resign}
     (let oppo (opponent mover))
     (board<- oppo squares castling #no oppo))

    ({.move-piece place0 place1 opt-en-passant-target}
     (let `(,r0 ,c0) place0)
     (let `(,r1 ,c1) place1)
     (let rows (array<-list (each array<-list squares)))
     (let piece ((rows r0) c0))
     ((rows r0) .set! c0 #\space)   ;; pretty clumsy
     ((rows r1) .set! c1 piece)

     ;; Update castling status if necessary
     (let new-castling castling)        ;XXX TODO

     (board<- (opponent mover) (each string<-list rows.values)
              new-castling opt-en-passant-target #no))

    ({.move-en-passant place0 place1}
     XXX)

    ({.move-promoting place0 place1}
     XXX)

    ({.castle}
     XXX)

    ({.play-turn `(,white-player ,black-player)}
     (let player (match mover
                   ('white white-player)
                   ('black black-player)))
     (let move (player board))
     (let possibles `({resign} ,@board.gen-legal-moves))  ;; isn't this poor factoring?
     (unless (possibles .find? move)
       (error "Bad move" move))
     (unless (= player human-player)
       (format "~w plays ~d.\n\n" mover (unparse-move move)))
     (update move board))

    ({.parse-move string}    ;N.B. #no if invalid, unlike in my Python
     ((map<-values unparse-move board.get-moves) .get string))

    ({.gen-legal-moves}
     (for those ((move board.gen-piece-moves))
       (not ((update move board) .checking?))))

    ({.get-moves}
     `({resign} ,@board.get-piece-moves))

    ({.get-piece-moves}
     board.gen-piece-moves)

    ({.gen-piece-moves}
     (let white? (= mover 'white))
     (for gather ((`(,r ,row) squares.items))
       (for gather ((`(,c ,piece) row.items))
         (if (and piece.letter? (= piece.uppercase? white?))
             (board .gen-moves-from r c piece.uppercase white?)
             '()))))

    ({.gen-moves-from r c piece white?}

     (to (takeable? r1 c1)
       (or (empty? r1 c1) (has-opponent? r1 c1)))

     (to (empty? r1 c1)
       (= #\space ((squares r1) c1)))

     (to (has-opponent? r1 c1)
       (let there ((squares r1) c1))
       (and there.letter? (not= there.uppercase? white?)))

     (to (move-to r1 c1 @(optional kind))
       {move `(,r ,c) `(,r1 ,c1) (or kind 'simple)})

     (to (pawn-move r1 c1 @(optional en-passant-target))
       (let back-rank (if white? 1 8))
       (if (= r1 back-rank)
           (move-to r1 c1 'pawn-promotion) ;TODO is this how I want to represent it?
           (move-to r1 c1 en-passant-target)))

     (to (move-freely dirs)
       (for gather ((`(,dr ,dc) dirs))
         (begin stepping ((i 1))
           (if (= i 9)
               '()
               (do (let r1 (+ r (* dr i)))
                   (let c1 (+ c (* dc i)))
                   (case ((empty? r1 c1)
                          (link (move-to r1 c1) (stepping (+ i 1))))
                         ((has-opponent? r1 c1)
                          (link (move-to r1 c1) '()))
                         (else
                          '())))))))

     (match piece

       (#\P
        ;; XXX do en passant too
        (let dr (if white? -1 1))
        (chain (if (empty? (+ r dr) c)
                   (link (move-to (+ r dr) c)
                         (if (and (= r (if white? 7 2))   ; initial 2 steps
                                  (empty? (+ r (* dr 2)) c))
                             (link (move-to (+ r (* dr 2)) c) '())
                             '()))
                   '())
               (if (has-opponent? (+ r dr) (- c 1))
                   `(,(move-to (+ r dr) (- c 1)))
                   '())
               (if (has-opponent? (+ r dr) (+ c 1))
                   `(,(move-to (+ r dr) (+ c 1)))
                   '())))

       (#\K
        (for filter ((`(,dr ,dc) queen-dirs))
          (let r1 (+ r dr))
          (let c1 (+ c dc))
          ;; XXX also castling
          (and (takeable? r1 c1)
               (move-to r1 c1))))

       (#\N
        (for filter ((`(,dr ,dc) knight-jumps))
          (let r1 (+ r dr))
          (let c1 (+ c dc))
          (and (<= 1 r1 8)
               (<= 1 c1 8)
               (takeable? r1 c1)
               (move-to r1 c1))))

       (#\Q (move-freely queen-dirs))
       (#\R (move-freely rook-dirs))
       (#\B (move-freely bishop-dirs))

       (#\space '())
       (#\-     '())))

    ({.squares}
     ("" .join squares))

    ))

(let rook-dirs   '(( 0  1) ( 0 -1) ( 1  0) (-1  0)))
(let bishop-dirs '((-1 -1) (-1  1) ( 1 -1) ( 1  1)))
(let queen-dirs  (chain rook-dirs bishop-dirs))

(let knight-jumps '(( 2  1) ( 2 -1) ( 1  2) ( 1 -2)
                    (-2  1) (-2 -1) (-1  2) (-1 -2)))

(to (opponent side)
  (match side
    ('white 'black)
    ('black 'white)))

(to (update move board)
  (match move
    ({resign}
     board.resign)
    ({move from-pos to-pos kind}
     (match kind
       ;; TODO we might need one more kind, like 'simple but with an en-passant-target
       ('simple
        (board .move-piece from-pos to-pos #no))
       ('castling
        (board .castle from-pos to-pos))
       ('en-passant-capture
        (board .move-en-passant from-pos to-pos))
       ('pawn-promotion
        (board .move-promoting from-pos to-pos))))))

(to (unparse-move move)
  (match move
    ({resign}
     "resign")
    ({move from-pos to-pos _}
     (chain (unparse-pos from-pos) (unparse-pos to-pos)))))

(to (unparse-pos `(,r ,c))
  ("~d~w" .format ("abcdefgh" (- c 1)) (- 9 r)))

(to (main<-rng rng)
  (let strategy-names
    (map<- `(("human"   ,human-player)
             ("greedy"  ,greedy-player)
             ("random"  ,(random-player<- rng))
             ("minimax" ,(minimax-player<- 2)))))
  (main<- strategy-names))
  
(export main<-rng)
