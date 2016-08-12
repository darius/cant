;; The N-queens problem as a BDD.
;; XXX untested

;; TODO design command-line interfacing
(define (main argv)
  (match argv
    ((_ n) (queens (number<-string n)))
    ((prog @_) (format "Usage: %d board-size" prog))))

(define (queens n)
  (match (satisfy-first (queens-problem n) 1)
    (#no (display "none\n"))
    (env (print-board n env))))

(define (print-board n env)
  (for each! ((row (make-board n)))
    (for each! ((var row))
      (format "%d " (".Q" (env var))))
    (newline)))

(define (queens-problem n)
  (conjoin (for each ((r (range<- n)))
             (disjoin (for each ((c (range<- n)))
                        (place-queen n r c))))))

(define (conjoin nodes) (foldr1 bdd-and nodes))
(define (disjoin nodes) (foldr1 bdd-or  nodes))

(define (place-queen n r c)

  (let env (map<-))

  (define (exclude rr cc)
    (when (and (<= 0 rr) (< rr n)
               (<= 0 cc) (< cc n))
;      (print `(exclude ,rr ,cc))
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

(define (match-env env)
  ;; Return a BDD that evaluates to 1 just when every variable in env
  ;; has its given value.
  ;; TODO checkme
  (for foldl ((tree lit1)
              ((var value) (sort env.items {reverse})))
    (if value
        (build-choice var lit0 tree)
        (build-choice var tree lit0))))

(define (queen n r c)
  ;; The variable for a queen at (row r, column c) in an n*n board.
  (+ 2 (* n r) c))  ;; vars must be >= 2

(define (make-board n)
  ;; Return a 2-d array of distinct variables: each means there's a
  ;; queen at its position. Row/column numbers start from 0.
  (for each ((r (range<- n)))
    (range<- (+ 2 (* r n)) (+ 2 (* (+ r 1) n)))))
