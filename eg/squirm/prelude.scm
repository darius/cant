;; Squirm code, not Squeam. These definitions go into the global
;; environment.

(to ((compose f g) x)  ;; TODO varargs
  (f (g x)))

(to (some ok? xs)
  (and (not (nil? xs))
       (or (ok? (first xs))
           (some ok? (rest xs)))))

(to (every ok? xs)
  (or (nil? xs)
      (and (ok? (first xs))
           (every ok? (rest xs)))))

(to (each! f xs)
  (unless (nil? xs)
    (f (first xs))
    (each! f (rest xs))))

(to (gather f xs)
  (for foldr ((x xs) (ys '()))
    (chain (f x) ys)))

(to (those ok? xs)
  (for foldr ((x xs) (ys '()))
    (if (ok? x) (link x ys) ys)))

(to (filter f xs)             ;TODO is this worth defining? good name?
  (those identity (each f xs)))

(to (foldl f z xs)
  (if (nil? xs)
      z
      (foldl f (f z (first xs)) (rest xs))))

(to (foldr f xs z)     ;TODO rename since args are in nonstandard order
  (if (nil? xs)
      z
      (f (first xs) (foldr f (rest xs) z))))

(to (foldr1 f xs)
  (let tail (rest xs))
  (if (nil? tail)
      (first xs)
      (f (first xs) (foldr1 f tail))))

(to (each f xs)
  (for foldr ((x xs) (ys '()))
    (link (f x) ys)))

(to (surely ok? msg)
  (unless ok?
    (exit msg)))
