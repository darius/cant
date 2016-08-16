;; Like nfa_simplest_set.py
;; using a first straw man of a set datatype.

(import (use "lib/hashset.scm") set<- union)

(define (re-match re chars)
  (let ending-states
    (for foldl ((states (re (set<- accept))) (c chars))
      (for foldr ((state states.keys) (new-states empty-set))
        (union (state c) new-states))))
  (ending-states .maps? accept))

(define (accept c) empty-set)
(define ((expect ch succs) c) (if (= ch c) succs empty-set))

(let empty-set (set<-))

(define ((lit<- ch) succs)    (set<- (expect ch succs)))
(define ((alt<- r s) succs)   (union (r succs) (s succs)))
(define ((chain<- r s) succs) (r (s succs)))
(define ((star<- r) succs)
  (let my-succs succs.diverge)
  (my-succs .union! (r my-succs))
  my-succs)


;; Parser

(import (use "lib/parson.scm")
        delay seclude either then invert feed maybe many
        empty lit-1 any-1 skip-any-1)

(let regex-parser
  (hide
   (let primary (delay (define (<primary>)
                         (seclude
                          (either (then (lit-1 #\() exp (lit-1 #\)))
                                  (then (invert (either (lit-1 #\))
                                                        (lit-1 #\|)
                                                        (lit-1 #\*)))
                                        any-1
                                        (feed (given (s) (lit<- (s 0))))))))))
   (let factor (seclude
                (then primary
                      (maybe (either (then (lit-1 #\*)
                                           (feed star<-)))))))
   (let term (delay (define (<term>)
                      (seclude
                       (then factor (many (then term
                                                (feed chain<-))))))))
   (let exp (delay (define (<exp>)
                     (seclude
                      (either (then term (many (then (lit-1 #\|) exp
                                                     (feed alt<-))))
                              empty)))))
   (then exp (invert skip-any-1))))

(define (parse-regex string)
  ((regex-parser string 0 0 '()) .result))


(export parse-regex re-match
        regex-parser  ;; TODO could be more abstract
        lit<- alt<- chain<- star<-)
