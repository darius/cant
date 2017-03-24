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

(to (those ok? xs)
  (for foldr ((x xs) (ys '()))
    (if (ok? x) (cons x ys) ys)))

(to (filter f xs)             ;TODO is this worth defining? good name?
  (those identity (each f xs)))

(to (remove xs unwanted) ;TODO different arg order? N.B. almost unused
  (for those ((x xs))
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
          (else `((,xs.first ,ys.first)
                  ,@(zipping xs.rest ys.rest))))))

(to (cons/lazy x thunk)
  (make lazy-list {extending list-trait}
    ({.empty?} #no)
    ({.first}  x)
    ({.rest}   (thunk))
    ;; XXX override parts of list-trait that need it for laziness
    ))

(to (those/lazy ok? xs)
  (if (ok? xs.first)
      (cons/lazy xs.first (given () (those/lazy ok? xs.rest)))
      (those/lazy ok? xs.rest)))

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

(let the-signal-handler (box<- panic))
(let the-last-error (box<- #no))

(to (install-signal-handler handler)
  (let parent-handler the-signal-handler.^)
  (the-signal-handler .^= (given (@evil)
                            (the-signal-handler .^= parent-handler)
                            (call handler evil))))

(to (with-fallback-signal-handler act)
  (let parent-handler the-signal-handler.^)
  (the-signal-handler .^= (given (k @evil)
                            (display "Error within error!\n")
                            (each! print evil)
                            (os-exit 1)))
  (act)
  (the-signal-handler .^= parent-handler))

(import
    (hide
      (to (qualify-exp context exp)
        (import (qualifier<- context) qe)
        (qe exp))
      
      (to (qualify-pat context pat)
        (import (qualifier<- context) qp)
        (qp exp))

      (to (qualifier<- context)

        (to (qe exp)
          (match exp
            ({constant _}   exp)
            ({variable _}   exp)
            ({make _ _ _ _} (qualify-make exp))
            ({do e1 e2}     {do (qe e1) (qe e2)})
            ({let p e}      {let (qp p) (qe e)})
            ({call e1 e2}   {call (qe e1) (qe e2)})
            ({term tag es}  {term tag (each qe es)})
            ({list es}      {list (each qe es)})))

        (to (qualify-make {make name stamp-exp trait-exp clauses})
          {make (qualify-name context name) (qe stamp-exp) (qe trait-exp)
                (hide
                  (import (qualifier<- (add-make-context context name))
                    qe qp)
                  (for each (((p p-vars e-vars e) clauses))
                    `(,(qp p) ,p-vars ,e-vars ,(qe e))))})

        (to (qp pat)
          (match pat
            ({any-pat}         pat)
            ({variable-pat v}  pat)
            ({constant-pat c}  pat)
            ({list-pat ps}     {list-pat (each qp ps)})
            ({term-pat tag ps} {term-pat tag (each qp ps)})
            ({view-pat e p}    {view-pat (qe e) (qp p)})
            ({and-pat p1 p2}   {and-pat (qp p1) (qp p2)})))

        (export qe qp))

      (to (qualify-name context name)
        (let parts (add-make-context context name))
        (let joined (":" .join parts))
        (let s (symbol<- joined))
        s)

      (to (add-make-context context name)
        ;; TODO make sure name is a symbol, or convert it
        `(,name.name ,@context))

      (export qualify-exp qualify-pat))

  qualify-exp qualify-pat)

(to (repl)                          ;TODO rename
  (import (use "lib/traceback") on-error-traceback)
  (begin interacting ()
    (the-signal-handler .^= (given (@evil)
                              (the-last-error .^= evil)
                              (call on-error-traceback evil)
                              (display "Enter (debug) for more.\n")
                              (interacting)))
    (display "sqm> ")
    (let sexpr (read))
    (unless (eof-object? sexpr)
      (let parsed (parse-exp sexpr))
      (let e (qualify-exp '() parsed))  ;TODO reverse the order of args
      (print (evaluate e '())) ;XXX reify a proper env object
      (interacting))))

(to (debug)
  (import (use "lib/debugger") inspect-continuation)
  (match the-last-error.^
    ((k @evil) (inspect-continuation k))
    (_ (display "No error to debug.\n"))))

(let the-modules (box<- '()))

;; To make it possible to reload a module by calling (use file-stem)
;; again afterward. N.B. that won't mutate the existing module object.
;; This is not very useful, though, because we still can't redefine
;; variables at the repl.
(to (unuse file-stem)                   ;TODO better name
  (the-modules .^= (for those (((stem mod) the-modules.^))
                     (not= stem file-stem))))

(to (use file-stem)                  ;TODO a realer module system
  ;; N.B. could sort of just use memoize if that were already loaded.
  (match (assoc file-stem the-modules.^)
    ((_ mod) mod)
    (#no
     (let mod (load (chain file-stem ".scm") `(,file-stem)))
     (the-modules .^= `((,file-stem ,mod) ,@the-modules.^))
     mod)))

(make load
  ((filename)
   (load filename '()))
  ((filename context)
   (let code `(hide ,@(with-input-file read-all filename)))
   (let code1 (parse-exp code))
   (let code2 (qualify-exp context code1))
   (evaluate code2 '())))

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
