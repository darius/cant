;; Tic-tac-toe game

(import (use 'memoize) memoize)
(import (use 'sturm) cbreak-mode render get-key cursor green)

(to (main `(,me ,@args))
  (let players
    (case (args.empty? (list<- human-play spock-play))
          ((= args.count 2) (each parse-player args)) ;TODO catch errors
          (else
           (format "Usage: ~d [X-player O-player]\n" me)
           (format "Available players: ~d\n"
                   (" " .join (sort player-registry.keys)))
           (os-exit 1))))
  (call tty-ttt `(,@players ,empty-grid)))

(to (quick-test)
  (tic-tac-toe spock-play spock-play {grid 0o610 0o061}))


;; Text-stream interface

(to (tic-tac-toe player opponent grid)
  (format "~d\n\n" (show grid))
  (case ((won? grid)   (format "~d wins.\n" (last-to-move grid)))
        ((drawn? grid) (format "A draw.\n"))
        (else
         (unless (`(,player ,opponent) .find? human-play)
           (format "~d to move ~d. (Press a key.)\n"
                   player.name (whose-move grid))
;           (get-key)                    ;XXX
           )
         (tic-tac-toe opponent player (player grid)))))


;; Graphical TTY interface

(to (tty-ttt player opponent grid)
  (for cbreak-mode ()
    (tty-playing player opponent grid)))

(to (tty-playing player opponent grid)

  (to (continue)
    (match (player grid)
      (#no
       (refresh ("~d (~d) resigns." .format player.name (whose-move grid))))
      (next-grid
       (tty-playing opponent player next-grid))))

  (to (refresh message)
    (ttt-render (show grid) message))

  (case ((won? grid)
         (refresh ("~d (playing ~d) wins."
                   .format opponent.name (last-to-move grid))))
        ((drawn? grid)
         (refresh "A draw."))
        ((= player.name "Human")
         (continue))
        (else
         (let quit?
           (and (not= opponent.name "Human")
                (do (refresh ("~d to move ~d. (Press a key; Q to quit.)"
                              .format player.name (whose-move grid)))
                    (= #\Q ((get-key) .uppercase)))))
         (unless quit?
           (refresh ("~d ponders..." .format player.name))
           (continue)))))

(to (ttt-render shown-grid message @(optional plaint))
  (render `(,(or plaint "") "\n\n" ,shown-grid "\n\n" ,message "\n\n")))

(make human-play
  ({.name} "Human")
  (`(,grid) 
   (let prompt ("~d move? [1-9; Q to quit] " .format (whose-move grid)))
   (begin asking ((plaint #no))
     (ttt-render (if plaint (show-with-moves grid) (show grid))
                 `(,prompt ,cursor)
                 plaint)
     (match ((get-key) .uppercase)
       (#\Q #no)
       (key (match (and (char? key)
                        (<= #\1 key #\9)
                        (update grid (move<-key key)))
              (#no (asking "Hey, that's not a move. Give me one of the digits below."))
              (successor successor)))))))

(to (show-with-moves grid)
  (each (highlight-if '.digit?) (show grid (1 .to 9))))

(to ((highlight-if special?) x)
  (if (special? x) (green x) x))

(to (move<-key digit-char)
  (- #\9 digit-char))


;; 'AI' players

(to (ai<- name evaluate)
  (make ai
    ({.name} name)
    (`(,grid) (min-by evaluate (successors grid)))))

(let spock-evaluate
  (memoize (given (grid)
             (if (won? grid)
                 -1
                 (match (successors grid)
                   ('() 0)
                   (succs (- (call min (each spock-evaluate succs)))))))))

(let spock-play (ai<- "Spock" spock-evaluate))

(let drunk-evaluate
  (memoize (given (grid)
             (if (won? grid)
                 -1
                 (match (successors grid)
                   ('() 0)
                   (succs (- (average (each drunk-evaluate succs)))))))))

(to (average numbers)
  (/ (sum numbers) numbers.count))

(let player-registry
  (for map<-values ((player `(,human-play
                              ,spock-play
                              ,(ai<- "Drunk" drunk-evaluate)
                              ,(ai<- "Max" (compound-key<- spock-evaluate
                                                           drunk-evaluate)))))
    player.name.lowercase))

(to (parse-player name)
  (player-registry name.lowercase))


;; Basic grid ops

(let empty-grid {grid 0 0})

(to (player-bits bits)
  (for each ((i (0 .to 8)))
    (1 .and (bits .>> i))))

(to (won? {grid p q})
  (for some ((way ways-to-win))
    (= way (way .and q))))

(let ways-to-win '(0o700 0o070 0o007 0o444 0o222 0o111 0o421 0o124))

(to (drawn? grid)
  ((successors grid) .empty?))

(to (successors grid)
  (for filter ((move (0 .to 8)))
    (update grid move)))

(to (update {grid p q} move)
  (let bit (1 .<< move))
  (and (= 0 (bit .and (p .or q)))
       {grid q (p .or bit)}))


;; The presentation layer

(to (whose-move grid)
  ((player-marks grid) 0))

(to (last-to-move grid)
  ((player-marks grid) 1))

(to (player-marks {grid p q})
  (if (= (sum (player-bits p))
         (sum (player-bits q)))
      "XO"
      "OX"))

(to (show {grid p q} @(optional opt-spaces))
  (let spaces (or opt-spaces ("." .repeat 9)))
  (let marks (player-marks {grid p q}))
  ;; TODO: could become a 'map union' operation on the lists
  (let values (for each ((slot (zip (reverse (player-bits p))
                                    (reverse (player-bits q))
                                    spaces)))
                (match slot
                  (`(1 0 ,_) (marks 0))
                  (`(0 1 ,_) (marks 1))
                  (`(0 0 ,s) s))))
  (call grid-format `{.format ,@values})) ;ugh

(let grid-format ("\n" .join ('(" ~d ~d ~d") .repeat 3)))


(export main quick-test)