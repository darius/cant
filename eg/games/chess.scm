;; Unfinished port of one-day-i-will-play-chess

(to ((main<- strategy-names) args)
  (display "(Moves look like 'e2e3')\n\n")
  (play-chess (strategy-names (args 1))
              (strategy-names (args 2))))

(to (play-chess white-strategy black-strategy)
  (play (initial-chess-board<-) `(,white-strategy ,black-strategy)))

(to (play board players)
  (begin playing ((board board))
    (print board)
    (unless board.outcome
      (display "\n\n")
      (playing (board .play-turn players)))))

(to (human-player board)
  (begin asking ()
    (display ("~d, your move? " .format (side-name board.mover)))
    (may stdin.read-line  ;TODO extract the stdin cap
      (be (? eof?) {resign})
      (be answer (or (board .parse-move answer)
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
  (let total (sum-by piece-values board.squares))
  ;; Just a little randomness makes play less boring:
  ;;XXX need to set up with an rng
  ;; total += random.uniform(0, 0.001)
  (may board.mover
    (be 'white (- total))
    (else      total)))

;; TODO not really tested
(to (minimax-evaluate board depth)
  (may depth
    (be 0 (greedy-evaluate board))
    (else (may board.get-piece-moves
            (be '() 0)
            (be moves (- (min @(for each ((move moves))
                                 (minimax-evaluate (update move board)
                                                   depth.-)))))))))

(let piece-values
  (map<- (_ #\space 0)
         (_ #\-     0)))
(for each! ((p "pnbrqk")
            (v '(10 31 33 50 90 10000)))
  (piece-values .set! p.lowercase v)
  (piece-values .set! p.uppercase (- v)))

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

    (to _.mover
      mover)

    (to (_ .selfie sink)
      (let lines (each (_ .slice 1 9)
                       (squares .slice 1 9)))
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
              (may board.outcome
                (be #no   ("~d to move" .format (side-name mover)))
                (be 'draw "A draw.")
                (be w     ("~d wins!" .format (side-name w))))))

    ;; Return #no, draw, black, or white (meaning the winner).
    (to _.outcome
      (hm (or outcome-param)
          (if board.checkmate?            (opponent mover))
          (if board.get-piece-moves.none? 'draw)
          (else                           #no)))

    ;; Is the player to move checkmated?
    (to _.checkmate?  ;;XXX are we getting this at the start? why?
      ;; Or: is the mover in check now, and in check after every possible move?
      ;; XXX also seems buggy in other ways, though these boards are hard to read in the console
      (to (in-check?)                    ;XXX redundant and untested. Not yet in the Python version.
        (let my-king (may mover
                       (be 'white #\K)
                       (be 'black #\k)))
        (let swap-players (board<- (opponent mover)
                                   squares castling en-passant-target outcome-param))
        (not (for every ((succ swap-players.gen-successors))
               (succ.squares .find? my-king))))
      (and (in-check?)
           (every _.checking? board.gen-successors)))

    ;; Is the opponent in check?
    (to _.checking?
      ;; Operationalized as: can the mover take the opposing king?
      (let opposing-king (may mover
                           (be 'white #\k)
                           (be 'black #\K)))
      (not (for every ((succ board.gen-successors))
             (succ.squares .find? opposing-king))))

    ;; The boards that can result from a move (other than resigning).
    (to _.gen-successors
      (for each ((move board.get-piece-moves))
        (update move board)))

    (to _.resign
      (let oppo (opponent mover))
      (board<- oppo squares castling #no oppo))

    (to (_ .move-piece place0 place1 ?en-passant-target)
      (let `(,r0 ,c0) place0)
      (let `(,r1 ,c1) place1)
      (let rows (array<-list (each array<-list squares)))
      (let piece ((rows r0) c0))
      ((rows r0) .set! c0 #\space)   ;; pretty clumsy
      ((rows r1) .set! c1 piece)

      ;; Update castling status if necessary
      (let new-castling castling)        ;XXX TODO

      (board<- (opponent mover) (each string<-list rows.values)
               new-castling ?en-passant-target #no))

    (to (_ .move-en-passant place0 place1)
      XXX)

    (to (_ .move-promoting place0 place1)
      XXX)

    (to _.castle
      XXX)

    (to (_ .play-turn `(,white-player ,black-player))
      (let player (may mover
                    (be 'white white-player)
                    (be 'black black-player)))
      (let move (player board))
      (let possibles (link {resign} board.gen-legal-moves))  ;; isn't this poor factoring?
      (unless (possibles .find? move)
        (error "Bad move" move))
      (unless (= player human-player)
        (format "~d plays ~d.\n\n" (side-name mover) (unparse-move move)))
      (update move board))

    (to (_ .parse-move string)    ;N.B. #no if invalid, unlike in my Python
      ((map<-values unparse-move board.get-moves) .get string))

    (to _.gen-legal-moves
      (for those ((move board.gen-piece-moves))
        (not (_.checking? (update move board)))))

    (to _.get-moves
      (link {resign} board.get-piece-moves))

    (to _.get-piece-moves
      board.gen-piece-moves)

    (to _.gen-piece-moves
      (let white? (= mover 'white))
      (for gather ((`(,r ,row) squares.items))
        (for gather ((`(,c ,piece) row.items))
          (if (and piece.letter? (= piece.uppercase? white?))
              (board .gen-moves-from r c piece.uppercase white?)
              '()))))

    (to (_ .gen-moves-from r c piece white?)

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
            (hm (if (= i 9)
                    '())
                (do (let r1 (+ r (* dr i)))
                    (let c1 (+ c (* dc i))))
                (if (empty? r1 c1)
                    (link (move-to r1 c1) (stepping i.+)))
                (if (has-opponent? r1 c1)
                    (link (move-to r1 c1) '()))
                (else
                    '())))))

      (may piece

        (be #\P
          ;; XXX do en passant too
          (let dr (if white? -1 1))
          (let r1 (+ r dr))
          (chain (if (empty? r1 c)
                     (link (move-to r1 c)
                           (if (and (= r (if white? 7 2))   ; initial 2 steps
                                    (empty? (+ r1 dr) c))
                               (link (move-to (+ r1 dr) c) '())
                               '()))
                     '())
                 (if (has-opponent? r1 c.-)
                     `(,(move-to r1 c.-))
                     '())
                 (if (has-opponent? r1 c.+)
                     `(,(move-to r1 c.+))
                     '())))

        (be #\K
          (for yeahs ((`(,dr ,dc) queen-dirs))
            (let r1 (+ r dr))
            (let c1 (+ c dc))
            ;; XXX also castling
            (and (takeable? r1 c1)
                 (move-to r1 c1))))

        (be #\N
          (for yeahs ((`(,dr ,dc) knight-jumps))
            (let r1 (+ r dr))
            (let c1 (+ c dc))
            (and (<= 1 r1 8)
                 (<= 1 c1 8)
                 (takeable? r1 c1)
                 (move-to r1 c1))))

        (be #\Q (move-freely queen-dirs))
        (be #\R (move-freely rook-dirs))
        (be #\B (move-freely bishop-dirs))

        (be #\space '())
        (be #\-     '())))

    (to _.squares
      ("" .join squares))

    ))

(let rook-dirs   '(( 0  1) ( 0 -1) ( 1  0) (-1  0)))
(let bishop-dirs '((-1 -1) (-1  1) ( 1 -1) ( 1  1)))
(let queen-dirs  (chain rook-dirs bishop-dirs))

(let knight-jumps '(( 2  1) ( 2 -1) ( 1  2) ( 1 -2)
                    (-2  1) (-2 -1) (-1  2) (-1 -2)))

(to (side-name side)
  side.name.capitalize)

(let opponent
  (given
    (be 'white 'black)
    (be 'black 'white)))

(to (update move board)
  (may move
    (be {resign}
      board.resign)
    (be {move from-pos to-pos kind}
      (may kind
        ;; TODO we might need one more kind, like 'simple but with an en-passant-target
        (be 'simple             (board .move-piece from-pos to-pos #no))
        (be 'castling           (board .castle from-pos to-pos))
        (be 'en-passant-capture (board .move-en-passant from-pos to-pos))
        (be 'pawn-promotion     (board .move-promoting from-pos to-pos))))))

(to (unparse-move move)
  (may move
    (be {resign}
      "resign")
    (be {move from-pos to-pos _}
      (chain (unparse-pos from-pos) (unparse-pos to-pos)))))

(to (unparse-pos `(,r ,c))
  ("~d~w" .format ("abcdefgh" c.-) (- 9 r)))

(to (main<-rng rng)
  (let strategy-names
    (map<- (_ "human"   human-player)
           (_ "greedy"  greedy-player)
           (_ "random"  (random-player<- rng))
           (_ "minimax" (minimax-player<- 2))))
  (main<- strategy-names))
  
(export main<-rng)
