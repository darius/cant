;; stdlib

(to (surely ok? @arguments)
  (unless ok?
    (call error (if arguments.empty? '("Assertion failed") arguments))))

(to (not= x y)
  (not (= x y)))

(make +
  (`() 0)
  (`(,a) a)
  (`(,a ,b) (a .+ b))
  (`(,a ,b ,@arguments) (foldl '.+ (a .+ b) arguments)))

(make *
  (`() 1)
  (`(,a) a)
  (`(,a ,b) (a .* b))
  (`(,a ,b ,@arguments) (foldl '.* (a .* b) arguments)))

(make -
  (`() (error "Bad arity"))
  (`(,a) (0 .- a))
  (`(,a ,b) (a .- b))
  (`(,a ,b ,@arguments) (foldl '.- (a .- b) arguments)))

(make-trait transitive-comparison compare?
  (`(,x ,@xs)
   (begin comparing ((x0 x) (xs xs))
     (match xs
       (`() #yes)
       (`(,x1 ,@rest) (and (compare? x0 x1)
                           (comparing x1 rest)))))))

(make <   {extending transitive-comparison} (`(,a ,b)      (= (compare a b) -1)))
(make <=  {extending transitive-comparison} (`(,a ,b) (not (= (compare a b)  1))))
(make <=> {extending transitive-comparison} (`(,a ,b)      (= (compare a b)  0))) ; XXX better name?
(make >=  {extending transitive-comparison} (`(,a ,b) (not (= (compare a b) -1))))
(make >   {extending transitive-comparison} (`(,a ,b)      (= (compare a b)  1)))

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
  (`(,a) a)
  (`(,a ,b) (if (< a b) a b))
  (`(,a ,b ,@rest) (call min `(,(min a b) ,@rest))))
(make max
  (`(,a) a)
  (`(,a ,b) (if (< a b) b a))
  (`(,a ,b ,@rest) (call max `(,(max a b) ,@rest))))

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
  (`() '())
  (`(,xs) xs)
  (`(,xs ,ys) (xs .chain ys))
  (`(,@arguments) (foldr1 '.chain arguments)))

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

(to (zip-with fn xs ys)
  (for each ((`(,x ,y) (zip xs ys)))
    (fn x y)))

;; TODO: name it (zip @rows) instead, like Python?
(to (transpose rows)
  (if (every '.empty? rows)   ; and make it (some '.empty? rows)?
      '()
      `(,(each '.first rows)
        ,@(transpose (each '.rest rows)))))

(to (intercalate between elements)      ;TODO unify with .join
  (if elements.empty?
      elements
      `(,elements.first
        ,@(for gather ((x elements.rest)) ;TODO more efficient
            `(,between ,x)))))

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
  (`(,limit)
   (range<- 0 limit))
  (`(,first ,limit)
   (if (<= limit first)
       '()
       (make range {extending list-trait}
         ({.empty?} #no)
         ({.first}  first)
         ({.rest}   (range<- (+ first 1) limit))
         ({.count}  (- limit first))
         (`(,i)
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
         )))
  (`(,first ,limit ,stride)
   (unless (< 0 stride)
     (error "TODO downward range" stride))
   (if (<= limit first)
       '()
       (make range {extending list-trait}
         ({.empty?} #no)
         ({.first}  first)
         ({.rest}   (range<- (+ first stride) limit stride))
         (`(,i)
          (error "TODO" range `(,i)))
         ({.maps? i}
          (error "TODO" range {.maps? i}))
         ))))

(make enumerate
  (`(,xs)
   (enumerate xs 0))
  (`(,xs ,i)
   (if xs.empty?
       '()
       (make enumeration {extending list-trait}
         ({.empty?} #no)
         ({.first}  `(,i ,xs.first))
         ({.rest}   (enumerate xs.rest (+ i 1)))))))

(to (sum ns)
  (foldl + 0 ns))

;; Split xs at its first element where split-point? is true.
;; That is, return `(,head ,tail), where (chain head tail) = xs,
;; and either tail is () or (split-point? tail.first) is true
;; at the first possible place.
(to (split-on split-point? xs)
  (begin scanning ((r-head '()) (xs xs))
    (if (or xs.empty? (split-point? xs.first))
        `(,(reverse r-head) ,xs)
        (scanning `(,xs.first ,@r-head) xs.rest))))

(to (array<- @elements)
  (array<-list elements))

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

(to (with-output-string take-sink)             ;TODO rename
  (let sink (string-sink<-))
  (take-sink sink)
  sink.output-string)

(let the-signal-handler (box<- panic))
(let the-last-error (box<- #no))

(to (push-signal-handler handler)
  (let parent-handler the-signal-handler.^)
  (to (handler/wrapped @evil)
    ;; TODO might be cleaner with unwind-protect actions spelled out inline
    (unwind-protect
     (to (handler-thunk)
       (the-signal-handler .^= parent-handler)
       (call handler evil))
     (to (unwind-thunk)
       (the-signal-handler .^= handler/wrapped))))
  (the-signal-handler .^= handler/wrapped))

(to (with-signal-handler handler thunk)
  (let parent-handler the-signal-handler.^)
  (the-signal-handler .^= (given (@evil)
                            (the-signal-handler .^= parent-handler)
                            (call handler evil)))
  (let result (thunk))
  (the-signal-handler .^= parent-handler)
  result)

(to (unwind-protect try finally)     ;TODO better name
  (let parent-handler the-signal-handler.^)
  (the-signal-handler .^= (to (unwind-protector @evil)
                            (the-signal-handler .^= parent-handler)
                            (finally)
                            (call parent-handler evil)))
  (let result (try))
  (the-signal-handler .^= parent-handler)
  (finally)
  result)

(to (fallback-signal-handler @evil)
  ;; XXX for some reason, the panic handler doesn't seem to be set
  ;; when we're in here. So, for now let's just set it:
  (the-signal-handler .^= panic)

  ;; N.B. we're trying not to use anything but primitives:
  (display "Error within error! Evils:\n")
  (to (printing xs)
    (unless xs.empty?
      (out .print xs.first)
      (out .display #\newline)
      (printing xs.rest)))
  (printing evil)
  (os-exit 1))

(to (break @values)
  (call error `("Breakpoint" ,@values)))

(to (system/must-succeed command)
  (unless (= 0 (system command))
    (error "Failed system command" command)))

(to (repl @(optional cmd-line-args))    ;TODO rename
  (import (use "lib/traceback") on-error-traceback)

  (let parent-handler the-signal-handler.^)
  (to (repl-handler @evil)
    (the-signal-handler .^= parent-handler)
    (the-last-error .^= evil)
    (call on-error-traceback evil)
    (display "Enter (debug) for more.\n")
    (interacting))

  (to (interacting)
    (the-signal-handler .^= repl-handler)
    (display "sqm> ")
    (let sexpr (read))
    (unless (eof? sexpr)
      (let e (parse-exp sexpr))
      (let value (evaluate e '())) ;XXX reify a proper env object
      (unless (= value void)
        (print value))
      (interacting))
    (newline))

  (match cmd-line-args
    (#no (interacting))
    ('() (interacting))
    (`(,filename ,@_)
     (the-signal-handler .^= repl-handler)     
     (load-and-run filename cmd-line-args)
     (interacting))))

(to (load-and-run filename args)
  (load filename `(,filename)) ;TODO remove .scm extension
  (when ((list-globals) .find? 'main)     ;XXX hack
    (main args)))

(to (debug)
  (import (use "lib/debugger") inspect-continuation)
  (match the-last-error.^
    (`(,k ,@evil) (inspect-continuation k))
    (_ (display "No error to debug.\n"))))

(let the-modules (box<- '()))

;; To make it possible to reload a module by calling (use file-stem)
;; again afterward. N.B. that won't mutate the existing module object.
;; This is not very useful, though, because we still can't redefine
;; variables at the repl.
(to (unuse file-stem)                   ;TODO better name
  (the-modules .^= (for those ((`(,stem ,mod) the-modules.^))
                     (not= stem file-stem))))

(to (use file-stem)                  ;TODO a realer module system
  ;; N.B. could sort of just use memoize if that were already loaded.
  (match (assoc file-stem the-modules.^)
    (`(,_ ,mod) mod)
    (#no
     (let mod (load-module (chain file-stem ".scm") `(,file-stem)))
     (the-modules .^= `((,file-stem ,mod) ,@the-modules.^))
     mod)))

(to (load filename @(optional context-arg))
  ;; XXX duplication
  (let context (or context-arg '()))
  (let code `(do ,@(with-input-file read-all filename)))
  (let code1 (parse-exp code context))
  (evaluate code1 '()))

(to (load-module filename @(optional context-arg))
  (let context (or context-arg '()))
  (let code `(hide ,@(with-input-file read-all filename)))
  (let code1 (parse-exp code context))
  (evaluate code1 '()))

(to (with-input-file fn filename)
  (let source (open-input-file filename))
  (let result (fn source))
  source.close                       ;TODO unwind-protect
  result)

(to (with-output-file fn filename)
  (let sink (open-output-file filename 'replace)) ;TODO the 'replace is for Chez
  (let result (fn sink))
  sink.close                       ;TODO unwind-protect
  result)

(to (read-all source)
  (let thing (read source))
  (if (eof? thing)
      '()
      (cons thing (read-all source))))

(the-signal-handler .^= fallback-signal-handler)
