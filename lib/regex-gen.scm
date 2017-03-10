;; Generate strings that match a regex.
;; From Udacity CS212 Python code by Peter Norvig.

;; Return the strings matching regex whose length is in Ns.
(to (generate regex Ns)
  (sort-by-key (regex Ns)       ;XXX sort-by-key
               (given (s) `(,s.count ,s))))

(let none   (set<-))
(let just-0 (set<- 0))

(to (literal str)
  (let just-str (set<- str))
  (given (Ns)
    (if (Ns .maps? str.count) just-str none)))

(to (star x)
  (let x1 (nonempty x))
  (begin x1* ()
    (given (Ns)
      ((maybe (then x1 (x1*))) Ns))))

(to (maybe x) (either empty x))

(to ((nonempty x) Ns) (x (Ns .difference just-0)))
(to ((either x y) Ns) ((x Ns) .union (y Ns)))

(to (one-of chars)
  (let set (call set<- chars))
  (given (Ns)
    (if (Ns .maps? 1) set null)))

(to ((then x y) Ns)
  ;; Return the set of matches to xy whose total length is in Ns. We
  ;; ask y only for lengths that are remainders after an x-match in
  ;; 0..max(Ns). (And we call neither x nor y if there are no Ns.)
  (case (Ns.empty? none)
        (else
         (let x-matches (x (call set<- (range<- (+ (call max Ns) 1)))))
         (let Ns-x (call set<- (each '.count x-matches)))
         (let Ns-y (call set<- (for gather ((n Ns.keys))
                                 (for gather ((m Ns-x.keys))
                                   (let r (- n m))
                                   (if (< r 0) '() `(,r))))))
         (let y-matches (y Ns-y))
         (call set<- (for gather ((m1 x-matches))
                       (for gather ((m2 y-matches))
                         (if (Ns .maps? (+ m1.count m2.count))
                             `(,(chain m1 m2))
                             '())))))))

(let dot     (one-of "?"))
(let empty   (literal ""))
(to (plus x) (then x (star x)))

(export
  empty literal then either star plus maybe
  one-of dot nonempty
  generate)
