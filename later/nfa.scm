;; Like nfa_simplest_set.py

(define (match re chars)
  (let ending-states
    (for foldl ((states (re (set<- accept))) (c chars)) ;XXX using fold over sets
      (for foldr ((state states) (new-states empty-set))
        (union (state c) new-states))))
  (ending-states .has? accept))

(define (accept c) empty-set)
(define ((expect ch succs) c) (if (= ch c) succs empty-set))

(let chain<- compose)
(define ((lit<- ch) succs)  (set<- (expect ch succs)))
(define ((alt<- r s) succs) (union (r succs) (s succs)))
(define ((star<- r) succs)
  (let my-succs (succs .diverge))
  (my-succs .union! (r my-succs))
  (my-succs .snapshot))
