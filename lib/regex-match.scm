;; Regular expression matching.
;; Like nfa_simplest_set.py using a first straw man of a set datatype.

;; Does regex match chars? (Anchored matching at both ends.)
(to (regex-match regex chars)
  (let ending-states
    (for foldl ((states (regex (set<- accept)))
                (c chars))
      (union-over (for each ((state states.keys))
                    (state c)))))
  (ending-states .maps? accept))

;; A state is a function from char to set of successor states.
(to (accept c)                    empty-set)
(to ((shift succs) c)             succs)
(to ((expect ch succs) c)         (if (= ch c)       succs empty-set))
(to ((expect-any-of chs succs) c) (if (chs .maps? c) succs empty-set))

(let empty-set (set<-))

;; A regex is a function from NFA to NFA. The input NFA represents the
;; 'rest of' the larger regex that this regex is part of; the output
;; NFA represents this regex followed by the rest. An NFA is represented
;; by a set of states, its start states. The input NFA might not be
;; fully constructed yet at the time we build the output, because of
;; the loop for the Kleene star -- so we need a mutable set.
;; XXX wrong again, argh.
(to (empty succs)         succs)
(to ((lit-char ch) succs) (set<- (expect ch succs)))
(to ((either r s) succs)  ((r succs) .union (s succs)))
(to ((then r s) succs)    (r (s succs)))
(to ((star r) succs)
  (let my-succs succs.copy)
  (my-succs .union! (r my-succs))
  my-succs)

;; Extras

(to (anyone succs) (set<- (shift succs)))
(to (one-of str)
  (let char-set (set<-list str))
  (given (succs)
    (set<- (expect-any-of char-set succs))))

(to (maybe r) (either empty r))
(to (plus r)  (then r (star r)))

(to (literal string)
  (foldr then (each lit-char string) empty))

;; Concrete syntax

(import (use 'regex-parse) regex-parser<-)

(let regex-parse
  (regex-parser<- (export
                    empty literal star then either plus maybe one-of anyone)))

(export
  regex-match regex-parse
  empty lit-char literal either then star
  plus maybe one-of anyone)
