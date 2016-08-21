;; stdlib

(define (assert ok? @arguments)
  (unless ok?
    (call error (if arguments.empty? '("Assertion failed") arguments))))

(define (not= x y)
  (not (= x y)))

(make +
  (() 0)
  ((a) a)
  ((a b) (a .+ b))
  ((a b @arguments) (foldl '.+ (a .+ b) arguments)))

(make *
  (() 1)
  ((a) a)
  ((a b) (a .* b))
  ((a b @arguments) (foldl '.* (a .* b) arguments)))

(make -
  (() (error "Bad arity"))
  ((a) (0 .- a))
  ((a b) (a .- b))
  ((a b @arguments) (foldl '.- (a .- b) arguments)))

;; TODO transitive multi-arg
(define (<   a b)      (= (a .compare b) -1))
(define (<=  a b) (not (= (a .compare b)  1)))
(define (<=> a b)      (= (a .compare b)  0)) ; XXX better name?
(define (>=  a b) (not (= (a .compare b) -1)))
(define (>   a b)      (= (a .compare b)  1))

;; XXX float contagion
(make min
  ((a) a)
  ((a b) (if (< a b) a b))
  ((a b @rest) (call min `(,(min a b) ,@rest))))
(make max
  ((a) a)
  ((a b) (if (< a b) b a))
  ((a b @rest) (call max `(,(max a b) ,@rest))))

(define (arg-min xs key) (foldr1 (given (x y) (if (< (key x) (key y)) x y))
                                 xs))
(define (arg-max xs key) (foldr1 (given (x y) (if (> (key x) (key y)) x y))
                                 xs))


;;XXX so should some of these be in list-trait?

(define (reverse xs)
  (for foldl ((ys '()) (x xs))
    (cons x ys)))

(define (foldl f z xs)
  (if xs.empty?
      z
      (foldl f (f z xs.first) xs.rest)))

(define (foldr f xs z)
  (if xs.empty?
      z
      (f xs.first (foldr f xs.rest z))))

(define (foldr1 f xs)
  (let tail xs.rest)
  (if tail.empty?
      xs.first
      (f xs.first (foldr1 f tail))))

(define (each f xs)
  (for foldr ((x xs) (ys '()))
    (cons (f x) ys)))

(define (gather f xs)
  (for foldr ((x xs) (ys '()))
    (chain (f x) ys)))

(define (filter ok? xs)
  (for foldr ((x xs) (ys '()))
    (if (ok? x) (cons x ys) ys)))

(define (remove xs unwanted)            ;TODO different arg order?
  (for filter ((x xs))
    (not= x unwanted)))

(define (list<- @arguments)
  arguments)

(make chain
  (() '())
  ((xs) xs)
  ((xs ys) (xs .chain ys))
  ((@arguments) (foldr1 '.chain arguments)))

(define (some ok? xs)
  (and (not xs.empty?)
       (or (ok? xs.first)
           (some ok? xs.rest))))

(define (every ok? xs)
  (or xs.empty?
      (and (ok? xs.first)
           (every ok? xs.rest))))

(define (each! f xs)
  (unless xs.empty?
    (f xs.first)
    (each! f xs.rest)))

(define (as-list seq)            ;XXX naming convention for coercions?
  (if seq.empty?
      '()
      (cons seq.first (as-list seq.rest))))

(define (zip xs ys)
  (match `(,xs ,ys)
    ((() ()) '())
    (((x @xs1) (y @ys1))
     `((,x ,y) ,@(zip xs1 ys1)))))

(define (cons/lazy x thunk)
  (make lazy-list {extending list-trait}
    ({.empty?} #no)
    ({.first}  x)
    ({.rest}   (thunk))
    ;; XXX override parts of list-trait that need it for laziness
    ))

(define (filter/lazy ok? xs)
  (if (ok? xs.first)
      (cons/lazy xs.first (given () (filter/lazy ok? xs.rest)))
      (filter/lazy ok? xs.rest)))

(define (gather/lazy f xs)
  (for foldr/lazy ((x xs)
                   (rest-thunk (given () '())))
    (chain/lazy (f x) rest-thunk)))

(define (chain/lazy xs ys-thunk)
  (foldr/lazy cons/lazy xs ys-thunk))

(define (foldr/lazy f xs z-thunk)
  (if xs.empty?
      (z-thunk)
      (f xs.first
         (given () (foldr/lazy f xs.rest z-thunk)))))

(define (identity x)
  x)

(define ((compose f g) @arguments)
  (f (call g arguments)))

(make range<-
  ((limit)
   (range<- 0 limit))
  ((first limit)
   (if (<= limit first)
       '()
       (make range {extending list-trait}
         ({.empty?} #no)
         ({.first}  first)
         ({.rest}   (range<- (+ first 1) limit))
         ({.count}  (- limit first))
         ((i)
          (if (not (integer? i))
              (error "Key error" range i)
              (do (let j (+ first i))
                  (if (and (<= first j) (< j limit))
                      j
                      (error "Out of range" range i)))))
         ({.maps? i}
          (and (integer? i)
               (do (let j (+ first i))
                   (and (<= first j) (< j limit)))))
         ))))

(make enumerate
  ((xs)
   (enumerate xs 0))
  ((xs i)
   (if xs.empty?
       '()
       (make enumeration {extending list-trait}
         ({.empty?} #no)
         ({.first}  `(,i ,xs.first))
         ({.rest}   (enumerate xs.rest (+ i 1)))))))

(define (sum ns)
  (foldl + 0 ns))

(define (vector<- @elements)
  (vector<-list elements))

(define (string<- @chars)
  (string<-list chars))

(define (method<- actor cue)
  (given (@arguments)
    (call actor (term<- cue arguments))))

(define (write x)                      ;TODO rename
  (out .print x))

(define (print x)                      ;TODO rename
  (write x)
  (newline))

(let the-signal-handler-box (box<- panic))

(define (repl)                          ;TODO rename
  (display "sqm> ")
  (print (evaluate (parse-exp (read)) '())) ;XXX reify a proper env object
  (repl))

(let the-modules (box<- '()))

(define (use filename)                  ;TODO a realer module system
  ;; N.B. could sort of just use memoize if that were already loaded.
  (match (assoc filename the-modules.^)
    ((_ mod) mod)
    (#no
     (let code (for with-input-file ((source filename))
                 `(hide ,@(read-all source))))
     (let mod (evaluate (parse-exp code) '()))
     (the-modules .^= `((,filename ,mod) ,@the-modules.^))
     mod)))

(define (with-input-file fn filename)
  (let source (open-input-file filename))
  (let result (fn source))
  source.close                       ;TODO unwind-protect
  result)

(define (read-all source)
  (let thing (read source))
  (if (eof-object? thing)
      '()
      (cons thing (read-all source))))
