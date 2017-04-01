;; The N-queens problem as a BDD.

(import (use "lib/bdd")
  bdd-and bdd-or satisfy-first
  lit0 lit1 build-choice)

(to (main args)
  (match args
    (`(,_ ,n) (queens (number<-string n)))
    (`(,prog ,@_) (format "Usage: ~d board-size" prog))))

(to (queens n)
  (match (satisfy-first (queens-problem n) 1)
    (#no (display "none\n"))
    (env (print-board n env))))

(to (print-board n env)
  (for each! ((row (board<- n)))
    (for each! ((var row))
      (format "~d " (".Q" (env var))))
    (newline)))

(to (queens-problem n)
  (conjoin (for each ((r (range<- n)))
             (disjoin (for each ((c (range<- n)))
                        (place-queen n r c))))))

(to (conjoin nodes) (foldr1 bdd-and nodes))
(to (disjoin nodes) (foldr1 bdd-or  nodes))

(to (place-queen n r c)

  (let env (map<-))

  (to (exclude rr cc)
    (when (and (<= 0 rr) (< rr n)
               (<= 0 cc) (< cc n))
      (env .set! (queen n rr cc) #no)))
  
  (for each! ((cc (range<- n)))
    (exclude r cc))
  (for each! ((rr (range<- n)))
    (exclude rr c))
  (for each! ((dd (range<- (+ (- n) 1) n)))
    (exclude (+ r dd) (+ c dd))
    (exclude (+ r dd) (- c dd))
    (exclude (- r dd) (+ c dd))
    (exclude (- r dd) (- c dd)))
  (env .set! (queen n r c) #yes)

  (match-env env))

(to (match-env env)                 ;TODO move this to bdd.scm?
  ;; Return a BDD that evaluates to 1 just when every variable in env
  ;; has its given value.
  (for foldl ((tree lit1)
              (`(,var ,value) (sort env.items {reverse})))
    (if value
        (build-choice var lit0 tree)
        (build-choice var tree lit0))))

(to (queen n r c)
  ;; The variable for a queen at (row r, column c) in an n*n board.
  (+ 2 (* n r) c))  ;; vars must be >= 2, to not clash with lits.

(to (board<- n)
  ;; Return a 2-d array of distinct variables: each means there's a
  ;; queen at its position. Row/column numbers start from 0.
  (for each ((r (range<- n)))
    (range<- (+ 2 (* r n)) (+ 2 (* (+ r 1) n)))))

(export queens queens-problem print-board)
