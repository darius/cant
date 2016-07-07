;; microKanren streams.

(define (unit value)
  (prepend value nothing))

(make nothing
  ({.+ m}    m)
  ({.>>= g}  nothing)
  ({.stream} '()))

(define (prepend value m)
  (make _
    ({.+ m1}   (prepend value (m .+ m1)))
    ({.>>= g}  ((g value) .+ (m .>>= g)))
    ({.stream} (cons-lazy value (given () (m .stream))))))

(define (lull thunk)
  (make _
    ({.+ m}    (lull (given () (m .+ (thunk)))))  ;; Note interleaving m before thunk.
    ({.>>= g}  (lull (given () ((thunk) .>>= g))))
    ({.stream} ((thunk) .stream))))

(define ((disjoin g1 g2) s/c)
  ((g1 s/c) .+ (g2 s/c)))

(define ((conjoin g1 g2) s/c)
  ((g1 s/c) .>>= g2))



;; Why not represent concatenations in constant time? Like, replacing
;; prepend with a unit constructor plus catenation constructor:
;; (plus lulls and interleaving as above, not shown)

(define (prepend value m)
  (catenate (unit value) m))

(make nothing
  ({.+ m}    m)
  ({.>>= g}  nothing))

(define (unit value)
  (make just-1
    ({.+ m}    (catenate just-1 m))
    ({.>>= g}  (g value))))

(define (catenate m1 m2)
  (make catenation
    ({.+ m3}  (catenate catenation m3))
    ({.>>= g} ((m1 .>>= g) .+ (m2 .>>= g)))))




;;XXX but what if m1 is ultimately nothing, through a chain of lulls?
;; Then we'd need to propagate the >>= to m2. So this is wrong.
;; How about defining some kind of "+>>=" method?
;; We can't just track whether we're nothing or something, because
;; we'd have to look inside thunks.
