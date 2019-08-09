;; Random tests from property checks.
;; Based on clickcheck/peckcheck.

;; Glossary:
;;    g    generator context: includes an RNG and a size
;;    gen  generates a value of a type, given a g

(import (use 'random) rng<-)
(let default-rng (rng<- 1234567))    ;TODO don't use this all the time

;; TODO report which property failed
;; TODO make and report deterministic seeds
;; TODO catch errors
;; TODO better names
(make all
  (to (_ property @gens)
    (all .run (context<- default-rng) 40 property gens))
  (to (_ .run g n-times property gens)
    (let failures (flexarray<-))
    (for each! ((_ (range<- n-times)))
      (let inputs (for each ((gen gens))
                    (gen g)))
      (may (property @inputs)
        (be #yes    (display "."))
        (be outcome (display "X")
                    (failures .push! `(,outcome ,inputs)))))
    (newline)
    (unless failures.empty?
      (format "Failures for ~w:\n" property)
      (for each! ((`(,outcome ,inputs) failures))
        (format "~w: ~w\n" outcome inputs)))
    failures.empty?))

(to (should be-ok? @arguments)
  (be-ok? @arguments))


;; Generator context

(make context<-
  (to (_ rng)
    (context<- rng 20))             ; TODO: is 20 a good default size?
  (to (_ rng size)
    ;; TODO better method names
    (make gen
      (to _.size           size)
      (to (_ .natural n)   (rng .random-integer n))
      (to (_ .range lo hi) (rng .random-range lo hi))
      (to _.a-size         (rng .random-integer size))
      (to (_ .choose xs)   (rng .pick xs)))))


;; Basic gens
;; TODO maybe name like <claim> etc.

(to (a-claim g)
  (may (g .natural 2)
    (be 0 #no)
    (be 1 #yes)))

(to (a-count g)
  (g .natural g.size))

(to (an-int g)
  (g .range (- g.size) g.size))

(to (a-char g)
  (char<- (g .natural 256)))

(to (a-printable-char g)
  (char<- (g .natural 32 126)))

(to ((a-list-of gen) g)
  (for each ((_ (range<- g.a-size)))
    (gen g)))

(to (a-string g)
  (string<-list ((a-list-of a-char) g)))

(to (a-printable-string g)
  (string<-list ((a-list-of a-printable-char) g)))

(to ((a-tuple @gens) g)
  (for each ((gen gens))
    (gen g)))

(to ((a-choice @gens) g)
  ((g .choose gens) g))


;; Helpers for gens

;; TODO extract something like this to random.scm
(to (weighted-choice choices)
  (let total (sum (each _.first choices)))
  (on (g)
    (begin scanning ((i (g .natural total))
                     (`((,weight ,choice) ,@rest) choices))
      (if (< i weight)
          choice
          (scanning (- i weight) rest)))))


(export
  a-claim a-count an-int a-char a-printable-char a-printable-string a-list-of a-tuple a-choice
  weighted-choice
  all should
  context<-
  )