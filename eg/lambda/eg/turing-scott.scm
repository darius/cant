;; Simulate Turing machines


;; Scott lists

(to (Snil if-link if-nil) if-nil)
(to (Slink h t if-link if-nil) (if-link h t))

(to (foldr f z)
  (fix ([folding xs]
        (xs ([h t]
             (f h (folding t)))
            z))))

(to (each f)
  (foldr (compose Slink f) Snil))

(to (each! f)
  (fix ([walking slist]
        (slist ([h t]
                (let _ (f h))
                (walking t))
               'ok))))

(let chain-reverse
  (fix ([rev xs ys]
        (xs ([x xs1]
             (rev xs1 (Slink x ys)))
            ys))))

(to (reverse slist)
  (chain-reverse slist Snil))


;; n-tuples: like Scott lists without the nil option.
;; You have to know how to stay short of the length by other means.

(to (Then f r k) (k f r))
(to (first tuple) (tuple ([f r] f)))
(to (rest tuple)  (tuple ([f r] r)))

(to (Tuple-Getter n)
  (compose first (church<-count n rest)))


;; maybe: if-none if-some -> result
;; if-none: result
;; if-some: value -> result

(to (None if-none if-some) if-none)
(to (Some x if-none if-some) (if-some x))


;; Tape representation
;; (I wonder how awful it'd be with Church lists instead?)
;; The left and right subtapes are Scott lists of squares; the left
;; one is represented reversed, and `head` is the square between them.
;; Each square is a tuple-getter that selects the square's printed
;; representation from an n-tuple, or the column from a transit table
;; (also an n-tuple).

(to (Tape left head right take)
  (take left head right))

(to (slist<-tape tape) 
  (tape ([left head right]
         (chain-reverse left (Slink head right)))))

(let square-displays (Then "-" (Then "#" error))) ;TODO don't assume binary squares

(to (show-square sq)
  (display (sq square-displays)))       ;a sq is a tuple-getter

(to (show-tape tape)
  (let _ (each! show-square (slist<-tape tape)))
  (display "\n"))

(to (init-tape left-nums head-num right-nums)
  (Tape (reverse (each Tuple-Getter left-nums))
        (Tuple-Getter head-num)
        (each Tuple-Getter right-nums)))


;; A machine has a transit function, a state, and a tape.

(to (Machine transit state tape take)
  (take transit state tape))

(to (show-config machine)
  (machine
   ([_ state tape]
    (let _ (show-tape tape))
    (tape ([L h R]
           (let _ (each! ([_] (display " ")) L))
           (display "*\n")              ;TODO show state and next move
           )))))

;; Update the machine config for one step
;; machine -> (Maybe machine)
(to (step machine)
  (machine ([transit state tape]
            (tape ([left head right]
                   ((transit state head)
                    None          ;; if none
                    ([answer]     ;; if some
                     (answer ([new-head move new-state]
                              (Some (Machine transit
                                             new-state
                                             (move left new-head right))))))))))))

(let default-square (Tuple-Getter 0))

;; Return a pair of slist's head and tail, if necessary extending it
;; with a default square.
(to (pop slist take)
  ;; thunkifying isn't strictly needed. TODO how much performance difference does it make?
  (let thunk (slist ([h t] ([_] (take h t)))
                    ([_] (take default-square Snil))))
  (thunk error))

(to (move-left left head right)
  (pop left ([h t] (Tape t h (Slink head right)))))

(to (move-right left head right)
  (pop right ([h t] (Tape (Slink head left) h t))))

;; State table makers
(to (Triple a b c take)
  (take a b c))

(to (Entry move square next-state)
  (Some (Triple (Tuple-Getter square) move (Tuple-Getter next-state))))

(let < (Entry move-left))
(let > (Entry move-right))

;; Make a transit function from a state table
(to (Transit transit-tuple state head)
  (head (state transit-tuple)))


;; Evaluating multiple steps

(to (loud-step maybe-machine)
  (maybe-machine None        ;; if none
                 ([machine]  ;; if some
                  (let _ (machine ([_ _ tape]
                                   (show-tape tape))))
                  (step machine))))

;; Run up to n steps
(to (stepping n machine)
  (church<-count n loud-step (Some machine)))

;; Run until halt (or forever)
(let loud-run
  (fix ([running machine]
        (let _ (show-config machine))
        (step machine
              'halt        ;; if-none
              running))))  ;; if-some


;; Example machine 1
;; https://en.wikipedia.org/wiki/Turing_machine_examples
;; "1. A machine can be constructed to compute the sequence 0 1 0 1 0 1..."
;; (0 <blank> 1 <blank> 0...)
;; (Undecidable p. 119)

(to (List2 a b) (Then a (Then b error)))

;; State           On tape 0    On tape 1
(let b       (List2 (> 0 1)      None))
(let c       (List2 (> 0 2)      None))
(let e       (List2 (> 1 3)      None))
(let f       (List2 (> 1 0)      None))
(let example-transit (Transit (Then b (Then c (Then e (Then f error))))))

(let example-tape (init-tape Snil 0 Snil))

(let example-machine (Machine example-transit (Tuple-Getter 0) example-tape))

(let _ (stepping 10 example-machine))

(let _ (display "\n\n"))


;; Example machine 2: copy subroutine
;; https://en.wikipedia.org/wiki/Turing_machine_examples

;; State           On tape 0      On tape 1
(let s0       (List2 None         (< 0 1)))  ;; Meaning go left, after writing 0 & transit to state 1
(let s1       (List2 (< 0 2)      (< 1 1)))
(let s2       (List2 (> 1 3)      (< 1 2)))
(let s3       (List2 (> 0 4)      (> 1 3)))
(let s4       (List2 (< 1 0)      (> 1 4)))
(let copy-machine-transit (Transit (Then s0 (Then s1 (Then s2 (Then s3 (Then s4 error)))))))

(let copy-machine-tape (init-tape (Slink 1 Snil) 1 Snil))

(let copy-machine (Machine copy-machine-transit (Tuple-Getter 0) copy-machine-tape))

(loud-run copy-machine)
