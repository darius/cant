;; Turing machine interpreter

;; mark = value that can appear in a tape square.
;; transit = map from `(,state ,mark) to `(,acts ,state).
;; act = '< | '> | mark
;;   (< = step left, > = right)

;; {tape ls head rs} where:
;; ls   = reverse of the list of tape squares to the left of the tape head.
;; head = the value of the square under the tape head.
;; rs   = the squares to the right of the tape head.
;; The lists ls and rs only extend so far as has been visited up to now.
;; Visited blank squares are symbolized as '-'.

(to (run machine @(optional option))
  (begin running ((machine machine))
    (when (= option 'loudly)
      (show-config machine)
      (newline))
    (may (step machine)
      (be #no     machine)
      (be updated (running updated)))))

(to (show-config {machine transit state {tape L h R}})
  (to (show-squares squares)
    (" " .join (for each ((s squares))
                 ("~w" .format s))))
  (let next-acts (may (transit .get `(,state ,h))
                   (be #no '())
                   (be `(,acts ,_) acts)))

  (let before (show-squares (reverse L)))
  (format "~d ~d ~d\n" before (show-squares `(,h)) (show-squares R))
  (format "~d ~w ~w\n" (" " .repeat before.count) state next-acts))

(to (step {machine transit state (and tape {tape _ head _})})
  (may (transit .get `(,state ,head))
    (be #no #no)
    (be `(,acts ,next-state) 
      {machine transit next-state (foldl perform tape acts)})))

(to (perform {tape L h R} act)
  (may act
    (be '<   {tape (L .slice 1) (L .get 0 '-) `(,h ,@R)})
    (be '>   {tape `(,h ,@L)    (R .get 0 '-) (R .slice 1)})
    (be mark {tape L mark R})))


;; Conveniences

;; TODO a canonicalizer that rewrites to an equiv turing machine
;; that steps one square at a time.

;; Make a transit function from a list of (state mark actions next-state)
(to (transit<- entries)
  (map<- (for each ((`(,state ,mark ,acts ,next-state) entries))
           `((,state ,mark) (,acts ,next-state)))))


;; Examples
;; https://en.wikipedia.org/wiki/Turing_machine_examples

;; "1. A machine can be constructed to compute the sequence 0 1 0 1 0 1..."
;; (0 <blank> 1 <blank> 0...)
;; (Undecidable p. 119)
(let turing-example-1
  '((b -  (0 >) c)
    (c -  (>)   e)
    (e -  (1 >) f)
    (f -  (>)   b)))

;; On the tape, 0 is represented by - (blank):
(let copy-subroutine
  '(
;;  (1 -  () halt)
    (s1 1  (- <) s2)
    (s2 -  (- <) s3)
    (s2 1  (1 <) s2)
    (s3 -  (1 >) s4)
    (s3 1  (1 <) s3)
    (s4 -  (- >) s5)
    (s4 1  (1 >) s4)
    (s5 -  (1 <) s1)
    (s5 1  (1 >) s5)
    ))

;; Compare to the trace at
;; https://en.wikipedia.org/wiki/Turing_machine_examples#A_copy_subroutine
(to (smoke-test)
  (let eg-copy {machine (transit<- copy-subroutine) 's1 '{tape (1) 1 ()}})
  (run eg-copy 'loudly))


(export
  run step show-config transit<-
  smoke-test)
