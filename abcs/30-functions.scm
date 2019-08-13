(make display
  (to (_ x)      (out .display x))
  (to (_ x sink) (sink .display x)))

(make newline
  (to (_)      (out .display #\newline))
  (to (_ sink) (sink .display #\newline)))

(to (yeah? x)
  (not= x #no))

;; XXX float contagion
(make min
  (to (_ a) a)
  (to (_ a b) (if (< a b) a b))
  (to (_ a b @rest) (min (min a b) @rest)))  ;TODO why aren't we using foldl? geez.
(make max
  (to (_ a) a)
  (to (_ a b) (if (< a b) b a))
  (to (_ a b @rest) (max (max a b) @rest)))

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
  (if elements.none?
      elements
      (link elements.first
            (for gather ((x elements.rest)) ;TODO more efficient
              `(,between ,x)))))

(to (link/lazy x thunk)
  (make lazy-list {extending list-trait}
    (to _.none? #no)
    (to _.first x)
    (to _.rest  (thunk))
    ;; XXX override parts of list-trait that need it for laziness
    ))

(to (those/lazy keep? xs)
  (if xs.none?
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
  (if xs.none?
      (z-thunk)
      (f xs.first
         (: (foldr/lazy f xs.rest z-thunk)))))

(to ((compose f g) @arguments)
  (f (g @arguments)))

(to (sum ns)
  (foldl + 0 ns))

(to (sum-by f xs) ;TODO overload 'sum' instead? (viz. other use of name below)
  (for foldl ((total 0) (x xs))
    (+ total (f x))))

(to (tally xs)
  (sum-by _.count xs))

(to (tally-by f xs)
  (sum-by (compose _.count f) xs))
;; TODO hm, I was thinking of f as returning a claim, but as written,
;; it could be any function that returns a countable thing, such as a
;; collection. What's a good name for this from that point of view?
;; total-count ? total ? sum-by ? count-by ?

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
    (if (or xs.none? (split-point? xs.first))
        `(,(reverse r-head) ,xs)
        (scanning (link xs.first r-head) xs.rest))))

(to (write x)                      ;TODO rename
  (out .write x))

(to (print x)                      ;TODO rename
  (write x)
  (newline))

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


;; Experiments

;: TODO better name 'with'?
(make take
  (to (_ input f)            ;for speed, a specialization of the below
    (f input))
  (to (_ input @transforms)
    (for foldl ((result input) (f transforms))
      (f result))))

(to (hey focus @actions)                ;TODO better name 'also'?
  (each! (-> (it focus)) actions)
  focus)

;; 'bind' on the maybe monad
(to (mayhap f ?thing)
  (may ?thing
    (be #no #no)
    (else   (f ?thing))))

;; probably worthless
(to (method<- actor cue)
  (on (@arguments)
    (call actor (term<- cue arguments))))
