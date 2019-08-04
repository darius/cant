;; Generate strings that match a regex.
;; From Udacity CS212 Python code by Peter Norvig.
;; Glossary:
;;    r, s   regex (i.e. a thing built by the constructors below)
;;    Ns     set of nonnegative integers (i.e. lengths)

;; Return the strings matching r whose length is in Ns.
(to (regex-generate r Ns)
  (sort-by (compound-key<- _.count identity)
           ((r Ns) .keys)))
           

(let none   (set<-))
(let just-0 (set<- 0))

(to (literal str)
  (let just-str (set<- str))
  (on (Ns)
    (if (Ns .maps? str.count) just-str none)))

(to (star r)
  (let r1 (nonempty r))
  (begin r1* ()
    (on (Ns) ((either empty (then r1 (r1*)))
              Ns))))

(to ((nonempty r) Ns) (r (Ns .difference just-0)))
(to ((either r s) Ns) ((r Ns) .union (s Ns)))

(to (one-of chars)
  (let set (set<-list (each string<- chars)))
  (on (Ns)
    (if (Ns .maps? 1) set none)))

(to ((then r s) Ns)
  ;; Return the set of matches to rs whose total length is in Ns. We
  ;; ask s only for lengths that are remainders after an r-match in
  ;; 0..max(Ns). (And we call neither r nor s if there are no Ns.)
  (hm (if Ns.empty? none)
      (else
       ;; TODO still pretty ugly
       (let r-matches (_.keys (r (_.range (0 .to (call max Ns.keys))))))
       (let r-lengths (_.keys (_.range (each _.count r-matches))))
       (let Ns-s (_.range (for gather ((n Ns.keys))
                            (for yeahs ((m r-lengths))
                              (and (<= m n) (- n m))))))
       (let s-matches (_.keys (s Ns-s)))
       (_.range (for gather ((m1 r-matches))
                  (for yeahs ((m2 s-matches))
                    (and (Ns .maps? (+ m1.count m2.count))
                         (chain m1 m2))))))))

;; Extras

(let anyone   (literal "?"))
(let empty    (literal ""))
(to (maybe r) (either empty r))
(to (plus r)  (then r (star r)))

;; Concrete syntax

(import (use 'regex-parse) regex-parser<-)

(let regex-parse
  (regex-parser<- (export
                    empty literal star then either plus maybe one-of anyone)))

(export
  empty literal then either star plus maybe
  one-of anyone nonempty
  regex-generate regex-parse)
