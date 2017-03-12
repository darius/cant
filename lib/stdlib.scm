;; stdlib

(to (surely ok? @arguments)
  (unless ok?
    (call error (if arguments.empty? '("Assertion failed") arguments))))

(to (not= x y)
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
(to (<   a b)      (= (compare a b) -1))
(to (<=  a b) (not (= (compare a b)  1)))
(to (<=> a b)      (= (compare a b)  0)) ; XXX better name?
(to (>=  a b) (not (= (compare a b) -1)))
(to (>   a b)      (= (compare a b)  1))

(to (compare a b)
  (let result (a .compare b))
  (if (comparison? result) result (error "Incomparable" a b)))

(to (comparison? x)
  (match x
    (-1 #yes)
    ( 0 #yes)
    (+1 #yes)
    (_  #no)))

;; XXX float contagion
(make min
  ((a) a)
  ((a b) (if (< a b) a b))
  ((a b @rest) (call min `(,(min a b) ,@rest))))
(make max
  ((a) a)
  ((a b) (if (< a b) b a))
  ((a b @rest) (call max `(,(max a b) ,@rest))))

(to (arg-min xs key) (foldr1 (given (x y) (if (< (key x) (key y)) x y))
                             xs))
(to (arg-max xs key) (foldr1 (given (x y) (if (> (key x) (key y)) x y))
                             xs))


;;XXX so should some of these be in list-trait?

(to (reverse xs)
  (for foldl ((ys '()) (x xs))
    (cons x ys)))

(to (foldl f z xs)
  (if xs.empty?
      z
      (foldl f (f z xs.first) xs.rest)))

(to (foldr f xs z)
  (if xs.empty?
      z
      (f xs.first (foldr f xs.rest z))))

(to (foldr1 f xs)
  (let tail xs.rest)
  (if tail.empty?
      xs.first
      (f xs.first (foldr1 f tail))))

(to (each f xs)
  (for foldr ((x xs) (ys '()))
    (cons (f x) ys)))

(to (gather f xs)
  (for foldr ((x xs) (ys '()))
    (chain (f x) ys)))

(to (filter ok? xs)
  (for foldr ((x xs) (ys '()))
    (if (ok? x) (cons x ys) ys)))

(to (remove xs unwanted)            ;TODO different arg order?
  (for filter ((x xs))
    (not= x unwanted)))

(to (list<- @arguments)
  arguments)

(make chain
  (() '())
  ((xs) xs)
  ((xs ys) (xs .chain ys))
  ((@arguments) (foldr1 '.chain arguments)))

(to (some ok? xs)
  (and (not xs.empty?)
       (or (ok? xs.first)
           (some ok? xs.rest))))

(to (every ok? xs)
  (or xs.empty?
      (and (ok? xs.first)
           (every ok? xs.rest))))

(to (each! f xs)
  (unless xs.empty?
    (f xs.first)
    (each! f xs.rest)))

(to (as-list seq)            ;XXX naming convention for coercions?
  (if seq.empty?
      '()
      (cons seq.first (as-list seq.rest))))

(to (zip xs ys)
  (to (mismatch)
    (error "zip: mismatched arguments" xs ys))
  (begin zipping ((xs xs) (ys ys))
    (case (xs.empty? (if ys.empty? '() (mismatch)))
          (ys.empty? (mismatch))
          (else (cons `(,xs.first ,ys.first)
                      (zipping xs.rest ys.rest))))))

(to (cons/lazy x thunk)
  (make lazy-list {extending list-trait}
    ({.empty?} #no)
    ({.first}  x)
    ({.rest}   (thunk))
    ;; XXX override parts of list-trait that need it for laziness
    ))

(to (filter/lazy ok? xs)
  (if (ok? xs.first)
      (cons/lazy xs.first (given () (filter/lazy ok? xs.rest)))
      (filter/lazy ok? xs.rest)))

(to (gather/lazy f xs)
  (for foldr/lazy ((x xs)
                   (rest-thunk (given () '())))
    (chain/lazy (f x) rest-thunk)))

(to (chain/lazy xs ys-thunk)
  (foldr/lazy cons/lazy xs ys-thunk))

(to (foldr/lazy f xs z-thunk)
  (if xs.empty?
      (z-thunk)
      (f xs.first
         (given () (foldr/lazy f xs.rest z-thunk)))))

(to (identity x)
  x)

(to ((compose f g) @arguments)
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

(to (sum ns)
  (foldl + 0 ns))

(to (vector<- @elements)
  (vector<-list elements))

(to (string<- @chars)
  (string<-list chars))

(to (method<- actor cue)
  (given (@arguments)
    (call actor (term<- cue arguments))))

(to (write x)                      ;TODO rename
  (out .print x))

(to (print x)                      ;TODO rename
  (write x)
  (newline))

(let the-signal-handler-box (box<- panic))
(let the-last-error (box<- #no))

(to (repl)                          ;TODO rename
  (import (use "lib/traceback") on-error-traceback)
  (begin interacting ()
    (the-signal-handler-box .^= (to (on-error-repl k @evil)
                                  (the-last-error .^= (cons k evil))
                                  (call on-error-traceback `(,k ,@evil))
                                  (display "Enter (debug) for more.\n")
                                  (interacting)))
    (display "sqm> ")
    (let sexpr (read))
    (unless (eof-object? sexpr)
      (print (evaluate (parse-exp sexpr) '())) ;XXX reify a proper env object
      (interacting))))

(to (debug)
  (import (use "lib/debugger") inspect-cont)
  (match the-last-error.^
    ((k @evil) (inspect-cont k))
    (_ (display "No error to debug.\n"))))

(let the-modules (box<- '()))

(to (use file-stem)                  ;TODO a realer module system
  ;; N.B. could sort of just use memoize if that were already loaded.
  (match (assoc file-stem the-modules.^)
    ((_ mod) mod)
    (#no
     (let mod (load (chain file-stem ".scm")))
     (the-modules .^= `((,file-stem ,mod) ,@the-modules.^))
     mod)))

(to (load filename)
  (let code (for with-input-file ((source filename))
              `(hide ,@(read-all source))))
  (evaluate (parse-exp code) '()))

(to (with-input-file fn filename)
  (let source (open-input-file filename))
  (let result (fn source))
  source.close                       ;TODO unwind-protect
  result)

(to (read-all source)
  (let thing (read source))
  (if (eof-object? thing)
      '()
      (cons thing (read-all source))))
