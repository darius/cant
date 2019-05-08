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
      (print (show-config machine)))
    (match (step machine)
      (#no machine)
      (updated (running updated)))))

(to (show-config {machine _ state {tape L h R}})
  `(,@(reverse L) (,state ,h) ,@R))

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

;; TODO a canonicalizer that rewrites to an equiv turing machine
;; that steps one square at a time.

;; TODO maybe: a transit maker that takes a list of (state mark actions next-state)
