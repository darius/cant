(to (yeah? x)
  (not= x #no))

;; XXX float contagion
(make min
  (`(,a) a)
  (`(,a ,b) (if (< a b) a b))
  (`(,a ,b ,@rest) (call min `(,(min a b) ,@rest))))
(make max
  (`(,a) a)
  (`(,a ,b) (if (< a b) b a))
  (`(,a ,b ,@rest) (call max `(,(max a b) ,@rest))))

(to (min-by key<- xs) (foldr1 (on (x y) (if (< (key<- x) (key<- y)) x y))
                              xs))
(to (max-by key<- xs) (foldr1 (on (x y) (if (> (key<- x) (key<- y)) x y))
                              xs))

(to ((compound-key<- @key-fns) x)   ;; TODO shorter name? combo-key? call-each?
  (for each ((f key-fns))
    (f x)))

;; Not sure this is the most useful design:
;;  - Most often we want result.range
;;  - Sometimes it's *almost* applicable, but ok? needs to take `(,i ,x) as the argument.
;;    But if we made it like that, then it's barely different from yeahs.
(to (where ok? xs)
  (for yeahs ((`(,i ,x) xs.items))
    (and (ok? x) i)))

(to (map-by f keys) ;TODO maybe name it map<-keys ? along with a map<-values ?
  (map<- (for each ((key keys))
           `(,key ,(f key)))))

(to (map<-values f values)
  (map<- (for each ((value values))
           `(,(f value) ,value))))

;; What's a good name for this? I like 'cartesian*' even less.
(to (grid* xs ys)                     ;TODO generalize
  (for gather ((x xs))
    (for each ((y ys))
      `(,x ,y))))

(to (intercalate between elements)      ;TODO unify with .join
  (if elements.empty?
      elements
      `(,elements.first
        ,@(for gather ((x elements.rest)) ;TODO more efficient
            `(,between ,x)))))

(to (link/lazy x thunk)
  (make lazy-list {extending list-trait}
    ({.empty?} #no)
    ({.first}  x)
    ({.rest}   (thunk))
    ;; XXX override parts of list-trait that need it for laziness
    ))

(to (those/lazy keep? xs)
  (if xs.empty?
      '()
      (if (keep? xs.first)
          (link/lazy xs.first (: (those/lazy keep? xs.rest)))
          (those/lazy keep? xs.rest))))

(to (each/lazy f xs)
  (for foldr/lazy ((x xs) (rest-thunk (: '())))
    (link/lazy (f x) rest-thunk)))

(to (gather/lazy f xs)
  (for foldr/lazy ((x xs) (rest-thunk (: '())))
    (chain/lazy (f x) rest-thunk)))

(to (chain/lazy xs ys-thunk)
  (foldr/lazy link/lazy xs ys-thunk))

(to (foldr/lazy f xs z-thunk)
  (if xs.empty?
      (z-thunk)
      (f xs.first
         (: (foldr/lazy f xs.rest z-thunk)))))

;; TODO maybe call this `count` -- too overloaded?
(to (tally f xs)
  (sum (each (compose '.count f) xs)))
;; TODO hm, I was thinking of f as returning a claim, but as written,
;; it could be any function that returns a countable thing, such as a
;; collection. What's a good name for this from that point of view?
;; total-count ? total ? sum-by ? count-by ?

(to ((compose f g) @arguments)
  (f (call g arguments)))

(to (sum ns)
  (foldl + 0 ns))

;; TODO too specialized for the stdlib
(to (union-over sets)
  (let result (set<-))
  (for each! ((set sets))
    (result .union! set))
  result)

;; Split xs at its first element where split-point? is true.
;; That is, return `(,head ,tail), where (chain head tail) = xs,
;; and either tail is () or (split-point? tail.first) is true
;; at the first possible place.
;; TODO I forgot this existed
(to (split-on split-point? xs)
  (begin scanning ((r-head '()) (xs xs))
    (if (or xs.empty? (split-point? xs.first))
        `(,(reverse r-head) ,xs)
        (scanning `(,xs.first ,@r-head) xs.rest))))

(to (method<- actor cue)
  (on (@arguments)
    (call actor (term<- cue arguments))))

(to (write x)                      ;TODO rename
  (out .print x))

(to (print x)                      ;TODO rename
  (write x)
  (newline))


;; Experiments

;;  TODO maybe also (take x y z (on (a b c) ...))
(to (take thing transform)
  (transform thing))

;; Maybe this should be syntax instead, for eval-order reasons.
(to (hey receiver @messages)            ;TODO maybe require at least one message
  (for foldl ((_ void) (m messages))
    (call receiver m)))

(to ((getter<- key) map)                ;better name?
  (map key))


;; Top level

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
  (the-signal-handler .^= (on (@evil)
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

  ;; N.B. we're trying to use only primitives here as far as possible:
  (display "Error within error! Evils:\n")
  (begin printing ((xs evil))
    (case (xs.empty? (os-exit 1))
          (else (out .print xs.first)
                (out .display #\newline)
                (printing xs.rest)))))

(to (breakpoint @values)
  (call error `("Breakpoint" ,@values)))

(to (system/must-succeed command)
  (unless (= 0 (system command))
    (error "Failed system command" command)))

(to (repl @(optional cmd-line-args))    ;TODO rename
  (import (use 'traceback) on-error-traceback)

  (let parent-handler the-signal-handler.^)

  (to (script-handler @evil)
    (the-signal-handler .^= parent-handler)
    (the-last-error .^= evil)
    (call on-error-traceback evil))

  (to (repl-handler @evil)
    (the-signal-handler .^= parent-handler)
    (the-last-error .^= evil)
    (call on-error-traceback evil)
    (display "Enter (debug) for more.\n")
    (interact))

  (to (interact)
    (the-signal-handler .^= repl-handler)
    (display "-> ")
    (match (read)
      ((? eof?) (newline))
      (sexpr (print-result (evaluate sexpr '()))))) ;XXX reify a proper env object

  ;; A separate function just to make the top of tracebacks cleaner.
  (to (print-result value)
    (unless (= value void)
      (print value))
    (interact))

  (match cmd-line-args
    (#no (interact))
    ('() (interact))
    (`("-i" ,filename ,@_)
     (the-signal-handler .^= repl-handler)
     (load-and-run filename cmd-line-args.rest)
     (interact))
    (`(,filename ,@_)
     (the-signal-handler .^= script-handler)
     (load-and-run filename cmd-line-args))))

(to (load-and-run filename args)
  (load filename `(,filename)) ;TODO remove .scm extension
  (when (global-defined? 'main)     ;XXX hack
    ((evaluate 'main '()) args)))

(to (debug)
  (import (use 'debugger) inspect-continuation)
  (match the-last-error.^
    (`(,k ,@evil) (inspect-continuation k))
    (_ (display "No error to debug.\n"))))

(let the-modules (map<-))

;; To make it possible to reload a module by calling (use file-stem)
;; again afterward. N.B. that won't mutate the existing module object.
(to (unuse file-stem)                   ;TODO better name
  (the-modules .delete! file-stem))

(to (use file-stem)                  ;TODO a realer module system
  ;; N.B. could sort of just use memoize if that were already loaded.
  (let stem (if (symbol? file-stem)
                (chain "lib/" file-stem.name)
                file-stem))
  (match (the-modules .get stem)
    (#no
     (let mod (load-module (chain stem ".scm") `(,stem)))
     (the-modules .set! stem mod)
     mod)
    (mod mod)))

(to (load filename @(optional context))
  (load-exp `(do ,@(with-input-file read-all filename))
            context))

(to (load-module filename @(optional context))
  (load-exp `(hide ,@(with-input-file read-all filename))
            context))

(to (load-exp exp context)
  (let code (parse-exp exp (or context '())))
  (evaluate code '()))

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

(to (read-all source) ;XXX confusing name, since source.read-all returns a string
  (let thing (read source))
  (if (eof? thing)
      '()
      (link thing (read-all source))))

(the-signal-handler .^= fallback-signal-handler)
