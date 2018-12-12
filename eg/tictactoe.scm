;; tic-tac-toe, as a warmup.

(import (use "lib/memoize") memoize)
(import (use "lib/sturm") cbreak-mode render get-key cursor green)

(to (main args)
  (let g {grid 0o000 0o000})
  (tty-ttt human-play spock-play g))

(to (quick-test)
  (let g {grid 0o610 0o061})
  (tic-tac-toe spock-play spock-play g))

(to (tty-ttt player opponent grid)
  (for cbreak-mode ()
    (tty-playing player opponent grid)))

(to (tty-playing player opponent grid)
  (to (continue)
    (tty-playing opponent player (player grid)))
  (to (render-grid message)
    (render `(,(tty-show grid) "\n\n" ,message)))
  (case ((won? grid)
         (render-grid ("~d wins." .format (last-to-move grid))))
        ((drawn? grid)
         (render-grid "A draw."))
        ((`(,player.name ,opponent.name) .find? 'Human)
         (continue))
        (else
         (render-grid ("~w to move ~d.\n(Press a key.)"
                       .format player.name (whose-move grid)))
         (unless (= (get-key) 'esc)  ;XXX sturm doesn't return 'esc
           ;; TODO erase the to-move msg while it's thinking
           (continue)))))

(make human-play
  ({.name} 'Human)
  (`(,grid) 
   (let prompt ("~d move? [1-9] " .format (whose-move grid)))
   (begin asking ((plaint #no))
     (render `(,(or plaint "")
               "\n\n"
               ,(if plaint (view-valid-moves grid) (tty-show grid))
               "\n\n"
               ,prompt ,cursor))
     (let key (get-key))
     (match (and (char? key)
                 (<= #\1 key #\9)
                 (apply-move grid (move<-key key)))
       (#no (asking "Hey, that's not a move. Give me one of the digits below."))
       (successor successor)))))

(to (move<-key char)
  (- 9 (number<-string (string<- char))))

(to (view-valid-moves grid)
  ;; TODO use fold
  (begin walking ((cs (tty-show grid))
                  (moves (1 .up-to 9)))
    (if cs.empty?
        '()
        (do (let c cs.first)
            (if c.whitespace?
                `(,cs.first ,@(walking cs.rest moves))
                (do (let blank? (= cs.first #\.))
                    `(,(if blank?
                           (green (string<-number moves.first)) ;ughish
                           c)
                      ,@(walking cs.rest moves.rest))))))))

(to (tic-tac-toe player opponent grid)
  (show grid)
  (newline)
  (case ((won? grid)   (format "~d wins.\n" (last-to-move grid)))
        ((drawn? grid) (format "A draw.\n"))
        (else
         (unless (`(,player ,opponent) .find? human-play)
           (format "~w to move ~d. (Press a key.)\n"
                   player (whose-move grid))
;           (get-key)                    ;XXX
           )
         (tic-tac-toe opponent player (player grid)))))

(make drunk-play
  ({.name} 'Drunk)
  (`(,grid) (arg-min (successors grid) drunk-value)))

(make spock-play
  ({.name} 'Spock)
  (`(,grid) (arg-min (successors grid) spock-value)))

(make max-play
  ({.name} 'Max)
  (`(,grid)
   (arg-min (successors grid)
            (given (succ) `(,(spock-value succ) ,(drunk-value succ))))))

(let drunk-value
  (memoize (given (grid)
             (if (won? grid)
                 -1
                 (match (successors grid)
                   ('() 0)
                   (succs (- (average (each drunk-value succs)))))))))

(let spock-value
  (memoize (given (grid)
             (if (won? grid)
                 -1
                 (match (successors grid)
                   ('() 0)
                   (succs (- (call min (each spock-value succs)))))))))

(to (average numbers)
  (/ (sum numbers) numbers.count))


(to (player-marks {grid p q})
  (if (= (sum (player-bits p))
         (sum (player-bits q)))
      "XO"
      "OX"))

(to (player-bits bits)
  (for each ((i (0 .up-to 8)))
    (1 .and (bits .>> i))))

(to (won? {grid p q})
  (for some ((way ways-to-win))
    (= way (way .and q))))

(to (drawn? grid)
  ((successors grid) .empty?))

(to (successors grid)
  (for filter ((move (0 .up-to 8)))
    (apply-move grid move)))

(to (apply-move {grid p q} move)
  (let bit (1 .<< move))
  (and (= 0 (bit .and (p .or q)))
       {grid q (p .or bit)}))

(to (whose-move grid)
  ((player-marks grid) 0))

(to (last-to-move grid)
  ((player-marks grid) 1))

(to (show {grid p q})
  (let marks (player-marks {grid p q}))
  (let values (for each ((pair (zip (player-bits p)
                                    (player-bits q))))
                (match pair
                  ('(1 0) (marks 0))
                  ('(0 1) (marks 1))
                  ('(0 0) #\.))))
  (call format `(,grid-format ,@(reverse values)))
  (newline))

(to (tty-show {grid p q})               ;XXX dupe
  (let marks (player-marks {grid p q}))
  (let values (for each ((pair (zip (player-bits p)
                                    (player-bits q))))
                (match pair
                  ('(1 0) (marks 0))
                  ('(0 1) (marks 1))
                  ('(0 0) #\.))))
  (call grid-format `{.format ,@(reverse values)})) ;ugh


(let grid-format ("\n" .join ('(" ~d ~d ~d") .repeat 3)))

(let ways-to-win '(0o700 0o070 0o007 0o444 0o222 0o111 0o421 0o124))

(let empty-grid {grid 0 0})

(export main quick-test)
