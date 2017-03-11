;; Regular expression matching.
;; Like nfa_simplest_set.py using a first straw man of a set datatype.

(import (use "lib/hashset") union-over)

;; Does regex match chars? (Anchored matching at both ends.)
(to (regex-match regex chars)
  (let ending-states
    (for foldl ((states (regex (set<- accept))) (c chars))
      (union-over (for each ((state states.keys)) (state c)))))
  (ending-states .maps? accept))

;; A state is a function from char to set of successor states.
(to (accept c) empty-set)
(to ((expect ch succs) c) (if (= ch c) succs empty-set))

(let empty-set (set<-))

;; A regex is a function from NFA to NFA. The input NFA represents the
;; 'rest of' the larger regex that this regex is part of; the output
;; NFA represents this regex followed by the rest. An NFA is represented
;; by a set of states, its start states. The input NFA might not be
;; fully constructed yet at the time we build the output, because of
;; the loop for the Kleene star -- so we need a mutable set.
(to (empty succs)         succs)
(to ((lit<- ch) succs)    (set<- (expect ch succs)))
(to ((alt<- r s) succs)   ((r succs) .union (s succs)))
(to ((chain<- r s) succs) (r (s succs)))
(to ((star<- r) succs)
  (let my-succs succs.diverge)
  (my-succs .union! (r my-succs))
  my-succs)

(export
  regex-match
  empty lit<- alt<- chain<- star<-)
