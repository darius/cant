;; tic-tac-toe, as a warmup.

(define (tic-tac-toe player opponent grid)
  (cond ((.won? grid)   (say (.last-to-move grid) " wins."))
        ((.drawn? grid) (say "A draw."))
        (else
         (unless (memq human-play `(,player ,opponent))
           (display (.show grid))
           (newline)
           (say (.show player) " to move " (.whose-move grid)
                ". (Press a key.)")
           (get-key))                    ;XXX
         (tic-tac-toe opponent player (player grid)))))

(define (human-play grid)
  "Just ask for a move."
  XXX)

(define (drunk-play grid)
  (minimum-by (.successors grid) drunk-value))

(define (spock-play grid)
  (minimum-by (.successors grid) spock-value))

(define (max-play grid)
  ; TODO: comparison of lists
  (minimum-by (.successors grid)
              (given (succ) `(,(spock-value succ) ,(drunk-value succ)))))

;; TODO: memoize
(define (drunk-value grid)
  (cond ((.won? grid) -1)
        (else
         (let succs (.successors grid))
         (if (.empty? succs)
             0
             (- (average (each drunk-value succs)))))))
      
(define (spock-value grid)
  (cond ((.won? grid) -1)
        (else
         (let succs (.successors grid))
         (if (.empty? succs)
             0
             (- (minimum (each spock-value succs)))))))

(define (average numbers)
  (/ (sum numbers) (.count numbers)))   ;TODO floats


(define (grid<- p q)

  (define (player-marks)
    (if (= (sum (player-bits p))
           (sum (player-bits q)))
        "XO"
        "OX"))

  (define (player-bits bits)
    (for each ((i (reverse (range<- 9)))) ;TODO: this is less efficient
      (.bit-and 1 (.>> bits i))))

  (make grid
    (.won? ()
      (for some ((way ways-to-win))
        (= way (.bit-and way q))))
    (.drawn? ()
      (.empty? (.successors grid)))
    (.successors ()
      (for filter-false ((move (range<- 9))) ;TODO better name
        (.move grid move)))
    (.move (move)
      (let bit (.<< 1 move))
      (and (= 0 (.bit-and bit (.bit-or p q)))
           (grid<- q (.bit-or p bit))))
    (.whose-move ()
      ((player-marks) 0))
    (.last-to-move ()
      ((player-marks) 1))
    (.show ()
      (let marks (player-marks))
      (call '.format grid-format
            (for each ((pair (zip (player-bits p) (player-bits q))))
              (case pair
                ((1 _) (marks 0))
                ((_ 1) (marks 1))
                (_     #\.)))))
    ))

(let grid-format (.join "\n" (for each ((_ (range<- 3)))
                               " %s %s %s")))

(let ways-to-win '(#o700 #o070 #o007 #o444 #o222 #o111 #o421 #o124))

(let empty-grid (grid<- 0 0))

(define (move<-human-numbered n)
  (- 9 n))

(define (sum ns)
  (foldl + 0 ns))

(define (filter-false xs)
  (filter identity xs))

(define (zip xs ys)
  (cond ((.empty? xs)
         (assert (.empty? ys) "Unequal list lengths" xs ys)
         '())
        ((.empty? ys)
         (error "Unequal list lengths" xs ys))
        (else
         (cons `(,(.first xs) ,(.first ys))
               (zip (.rest xs) (.rest ys))))))


(define (identity x)
  x)

(let say XXX)
