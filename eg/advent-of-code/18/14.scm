(let input (with-input-file read "eg/advent-of-code/18/data/advent14"))
;(let input 9)

(display "\nPart 1\n")

(let board (flexarray<- 3 7))
(let elves '#(0 1))

(to (step!)
  (board .extend! (digits<- (sum (each at elves))))
  (for each! ((`(,i ,elf) elves.items)) ;maybe a method like array .update!
    (elves .set! i (coord (+ elf 1 (at elf))))))

(to (at elf)
  (board (coord elf)))

(to (coord elf)
  (elf .modulo board.count))

(to (digits<- n)
  (each (compose number<-string string<-) (string<-number n)))  ; a little clumsy

(to (part-1)
  (let limit (+ input 10))
  (begin working ()
;    (show)
    (when (< board.count limit)
      (step!)
      (working)))
  (let digits (board .slice input))
  ("" .join (each string<-number digits)))

(to (show)
  (for each! ((`(,i ,recipe) board.items))
    (let f 
      (case ((= i (coord (elves 0))) "(~w)")
            ((= i (coord (elves 1))) "[~w]")
            (else                    " ~w ")))
    (display (f .format recipe)))
  (newline))

(format "~w\n" (part-1))


(display "\nPart 2\n")
(print input)

;; takes over 2 hrs
(to (part-2)
  (let pattern (call array<- (digits<- input)))
  (let pc pattern.count)
  (format "pattern: ~w\n" pattern)
  (begin working ((i 0))
    (when (1000 .divides? i)
      (format "working ~w\n" i))
;    (show)
    (begin checking ((j i))
      (if (< (+ j pc) board.count)
          (do (when (for every ((`(,k ,p) pattern.items))   ;(= pattern (board .slice j (+ j pc)))   ; ugh
                      (= p (board (+ j k))))
                (print j)
                (os-exit 0))
              (checking (+ j 1)))
          (do (step!)
              (working j))))))

(part-2)
