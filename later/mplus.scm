;; microKanren streams.

(to (unit state)
  (prepend state nothing))

(make nothing
  ({.+ m}    m)
  ({.>>= g}  nothing)
  ({.stream} '()))

(to (prepend state m)
  (make prepended
    ({.+ m1}   (prepend state (m .+ m1)))
    ({.>>= g}  ((g state) .+ (m .>>= g)))
    ({.stream} (cons-lazy state (given () m.stream)))))
;;XXX isn't .stream a dumb bag on the side of it?
;; But it seems needed to ever force a value out of a lull.

(to (lull<- thunk)
  (make lull
    ({.+ m}    (lull<- (given () (m .+ (thunk)))))  ;; Note interleaving m before thunk.
    ({.>>= g}  (lull<- (given () ((thunk) .>>= g))))
    ({.stream} ((thunk) .stream))))

(to ((either<- g1 g2) state)
  ((g1 state) .+ (g2 state)))

(to ((both<- g1 g2) state)
  ((g1 state) .>>= g2))



;; Why not represent concatenations in constant time? Like, replacing
;; prepend with a unit constructor plus catenation constructor:
;; (plus lulls and interleaving as above, not shown)

(to (prepend value m)
  (catenate (unit value) m))

(make nothing
  ({.+ m}    m)
  ({.>>= g}  nothing)
  ({.stream} '()))

(to (unit value)
  (make just-1
    ({.+ m}    (catenate just-1 m))
    ({.>>= g}  (g value))
    ({.stream} `(,value))))

(to (catenate m1 m2)
  (make catenation
    ({.+ m3}   (catenate catenation m3))
    ({.>>= g}  ((m1 .>>= g) .+ (m2 .>>= g)))
    ({.stream} (chain/lazy m1.stream (given () m2.stream)))))



;;XXX but what if m1 is ultimately nothing, through a chain of lulls?
;; Then we'd need to propagate the >>= to m2. So this is wrong.
;; How about defining some kind of "+>>=" method?
;; We can't just track whether we're nothing or something, because
;; we'd have to look inside thunks.
