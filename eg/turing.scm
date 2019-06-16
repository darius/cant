;; Turing machine interpreter
;; XXX untested

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
    (match (step machine)
      (#no machine)
      (updated (running updated)))))

(to (show-config {machine transit state {tape L h R}})
  (to (show-squares squares)
    (" " .join (for each ((s squares))
                 ("~w" .format s))))
  (let next-acts (match (transit .get `(,state ,h))
                   (#no '())
                   (`(,acts ,_) acts)))

  (let before (show-squares (reverse L)))
  (format "~d ~d ~d\n" before (show-squares `(,h)) (show-squares R))
  (format "~d ~w ~w\n" (" " .repeat before.count) state next-acts))

(to (step {machine transit state (and tape {tape _ head _})})
  (match (transit .get `(,state ,head))
    (#no #no)
    (`(,acts ,next-state) 
     {machine transit next-state (foldl perform tape acts)})))

(to (perform {tape L h R} act)
  (match act
    ('<   (let `(,l0 ,@ls) (peek L)) {tape ls l0 `(,h ,@R)})
    ('>   (let `(,r0 ,@rs) (peek R)) {tape `(,h ,@L) r0 rs})
    (mark {tape L mark R})))

(to (peek marks)
  (if marks.empty? '(-) marks))


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
