;; Like nfa_simplest_set.py
;; with a first straw man of a set datatype.

(define (re-match re chars)
  (let ending-states
    (for foldl ((states (re (set<- accept))) (c chars))
      (for foldr ((state states.keys) (new-states empty-set))
        (union (state c) new-states))))
  (ending-states .maps? accept))

(define (accept c) empty-set)
(define ((expect ch succs) c) (if (= ch c) succs empty-set))

(let chain<- compose)
(define ((lit<- ch) succs)  (set<- (expect ch succs)))
(define ((alt<- r s) succs) (union (r succs) (s succs)))
(define ((star<- r) succs)
  (let my-succs succs.diverge)
  (my-succs .union! (r my-succs))
  my-succs)

(make set<-             ;XXX this name is better saved for frozen sets
  (() (hash-set<-))
  ((val @vals) ;XXX would be nicer as (@vals) but that still matches non-lists
   (let s (hash-set<-))
   (s .add-all! `(,val ,@vals))
   s))

(define (hash-set<-)
  (let map (map<-)) ;TODO would be nice to avoid storing all the #yes values
  (make hash-set
    ({.keys}          map.keys)
    ({.maps? key}     (map .maps? key))
    ({.diverge}       (call set<- map.keys)) ;TODO tune
    ({.add! key}      (map .set! key #yes))
    ({.add-all! vals} (for each! ((v vals)) (hash-set .add! v)))
    ({.union! other}  (hash-set .add-all! other.keys))
    ({.empty?}        map.empty?)
    ;; XXX fill in rest of set interface (just the map interface, I guess)
    ))

(let empty-set (set<-))

(define (union set1 set2)      ;XXX name clash with lambdacompiler.scm
  (let result set1.diverge)
  (result .union! set2)
  result)


;; Parser
;; XXX buggy

(let regex-parser
  (hide
   (let primary (delay (given ()
                         (seclude
                          (either (then (lit-1 #\() exp (lit-1 #\)))
                                  (then any-1 (feed lit<-))))))) ;XXX any-1 is overly permissive
   (let factor (seclude
                (then primary
                      (maybe (either (then (lit-1 #\*) (feed star<-)))))))
   (let term (delay (given ()
                      (seclude
                       (then factor (many (then term (feed chain<-))))))))
   (let exp (delay (given ()
                     (seclude
                      (either (then term (many (then (lit-1 #\|) exp (feed alt<-))))
                              empty)))))
   (then exp (invert skip-any-1))))

(define (parse-regex string)
  ((regex-parser string 0 0 '()) .result))


; TODO more tests

(print (re-match (lit<- #\A) "hello"))
(print (re-match (lit<- #\A) "A"))

;(print (re-match (parse-regex "B") "B"))
