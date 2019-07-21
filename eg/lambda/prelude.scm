;; Prelude, built-ins

(to (build-prelude interpret lookup apply)

  (let prelude-sans-builtins
    '(do 

       (to (no if-yes if-no) if-no)
       (to (yes if-yes if-no) if-yes)
       (to (claim<-church p) (p #yes #no))

       (to (if pred y n) (pred y n))
       (to (and p q) (if p q no))
       (to (or p q) (if p yes q))
       (to (not1 p) (p no yes)) ;; Right? Wikipedia sez "only correct for normal order". Why?
       (to (not p a b) (p b a))  ;; What they say is right for applicative order.

       (to (zero f x) x)
       (to (succ n f x) (f (n f x)))

       (to (+ m) (m succ))
       (to (* m n) (m (+ n) zero))
       (to (expt m n) (n m))

       (to (nil if-link if-nil) if-nil)
       (to (link h t if-link if-nil) (if-link h (t if-link if-nil)))

       (to (chain xs ys) (xs link ys))

       (to (compose f g x) (f (g x)))
       (to (identity x) x)

       (to (fix maker)
         (to (r recur) (maker ([x] (recur recur x))))
         (r r))

       (the-environment)))

  (let prelude-sans-builtins-env (interpret prelude-sans-builtins {empty-env}))

  (let lc-no  (lookup prelude-sans-builtins-env 'no))
  (let lc-yes (lookup prelude-sans-builtins-env 'yes))

  (to (church<-claim claim)
    (match claim
      (#no  lc-no)
      (#yes lc-yes)))

  (let lc-zero (lookup prelude-sans-builtins-env 'zero))
  (let lc-succ (lookup prelude-sans-builtins-env 'succ))

  (to (church<-count n)
    (begin counting ((n n))
      (match n
        (0          lc-zero)
        ((? count?) (apply lc-succ (counting (- n 1)))))))

  (let lc-nil  (lookup prelude-sans-builtins-env 'nil))
  (let lc-link (lookup prelude-sans-builtins-env 'link))

  (to (church<-list xs)
    (begin linking ((xs xs))
      (match xs
        ('()       lc-nil)
        (`(,h ,@t) (apply (apply lc-link h) (linking t))))))

  (let builtins-env
    {module
     (map<- `(
              (church<-claim {primitive ,church<-claim})

              (add1 {primitive ,(given (n) (+ n 1))})
              (church<-count {primitive ,church<-count})

              (squeam-link {primitive ,(given (h)
                                         {primitive (given (t) (link h t))})})
              (church<-list {primitive ,church<-list})

              (display {primitive ,display})
              (error {primitive ,(make lambda-error
                                   (`(,_) (error "Error in lambda-calculus program")))})
              ))
     prelude-sans-builtins-env})

  (let prelude-using-builtins
    '(do 
       (to (count<-church n) (n add1 0))
       (to (list<-church xs) (xs squeam-link '()))
       (the-environment)))

  (interpret prelude-using-builtins builtins-env))

(export build-prelude)
