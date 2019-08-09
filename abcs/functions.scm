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
  (if elements.empty?
      elements
      (link elements.first
            (for gather ((x elements.rest)) ;TODO more efficient
              `(,between ,x)))))

(to (link/lazy x thunk)
  (make lazy-list {extending list-trait}
    (to _.empty? #no)
    (to _.first  x)
    (to _.rest   (thunk))
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
  (sum (each (compose _.count f) xs)))
;; TODO hm, I was thinking of f as returning a claim, but as written,
;; it could be any function that returns a countable thing, such as a
;; collection. What's a good name for this from that point of view?
;; total-count ? total ? sum-by ? count-by ?

(to ((compose f g) @arguments)
  (f (g @arguments)))

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
        (scanning (link xs.first r-head) xs.rest))))

(to (write x)                      ;TODO rename
  (out .write x))

(to (print x)                      ;TODO rename
  (write x)
  (newline))


;; Experiments

;;  TODO maybe also (take x y z (on (a b c) ...))
(to (take thing transform)
  (transform thing))

(to (hey focus @actions)                ;TODO: better name 'with'?
  (for each! ((act actions))
    (act focus))
  focus)

;; probably worthless
(make method<-
  (to (_ cue)
    (on (actor @arguments)
      (call actor (term<- cue arguments))))
  (to (_ actor cue)
    (on (@arguments)
      (call actor (term<- cue arguments)))))
