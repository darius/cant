(define (fail s)
  '())

(define (succeed s)
  `(,s))

(define ((== val1 val2) s)
  (let s1 (unify s val1 val2))
  (if s1 `(,s1) '()))

(define ((either goal1 goal2) s)
  (interleave (goal1 s) (goal2 s)))

;; TODO: probably ought to be lazy in the head as well as the tail
(define (interleave xs ys)
  (if (.empty? xs)
      ys
      (cons/lazy (.first xs)
                 (given () (interleave ys (.rest xs))))))

(define ((both goal1 goal2) s)
  (each-chained/lazy goal2 (goal1 s)))

(define (cons/lazy x thunk)
  (make lazy-list
    (.empty? () #no)
    (.first () x)
    (.rest () (thunk))  ;XXX memoize?
    ;; ...
    ))

(define (each-chained/lazy f xs)
  (foldr/lazy (given (x rest-thunk) (chain/lazy (f x) rest-thunk))
              (given () '())
              xs))

(define (chain/lazy xs ys-thunk)
  (foldr/lazy cons/lazy ys-thunk xs))

(define (foldr/lazy f z-thunk xs)
  (if (.empty? xs)
      (z-thunk)
      (f (.first xs)
         (given () (foldr/lazy f (.rest xs))))))
