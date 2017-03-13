;; Generate strings that match a regex.
;; From Udacity CS212 Python code by Peter Norvig.
;; Glossary:
;;    r, s   regex (i.e. a thing built by the constructors below)
;;    Ns     set of nonnegative integers (i.e. lengths)

;; Return the strings matching r whose length is in Ns.
(to (generate r Ns)
  (sort-by-key (r Ns)
               (given (str) `(,str.count ,str))))

(let none   (set<-))
(let just-0 (set<- 0))

(to (literal str)
  (let just-str (set<- str))
  (given (Ns)
    (if (Ns .maps? str.count) just-str none)))

(to (star r)
  (let r1 (nonempty r))
  (begin r1* ()
    (given (Ns) ((either empty (then r1 (r1*)))
                 Ns))))

(to ((nonempty r) Ns) (r (Ns .difference just-0)))
(to ((either r s) Ns) ((r Ns) .union (s Ns)))

(to (one-of chars)
  (let set (call set<- (as-list chars)))
  (given (Ns)
    (if (Ns .maps? 1) set null)))

(to ((then r s) Ns)
  ;; Return the set of matches to rs whose total length is in Ns. We
  ;; ask s only for lengths that are remainders after an r-match in
  ;; 0..max(Ns). (And we call neither r nor s if there are no Ns.)
  (case (Ns.empty? none)
        (else
         (let r-matches (r (call set<- (range<- (+ (call max Ns) 1)))))
         (let Ns-r (call set<- (each '.count r-matches)))
         (let Ns-s (call set<- (for gather ((n Ns.keys))
                                 (for filter ((m Ns-r.keys))
                                   (let r (- n m))
                                   (and (<= 0 r) r)))))
         (let s-matches (s Ns-s))
         (call set<- (for gather ((m1 r-matches))
                       (for filter ((m2 s-matches))
                         (and (Ns .maps? (+ m1.count m2.count))
                              (chain m1 m2))))))))

(let dot      (one-of "?"))
(let empty    (literal ""))
(to (maybe r) (either empty r))
(to (plus r)  (then r (star r)))

(export
  empty literal then either star plus maybe
  one-of dot nonempty
  generate)
