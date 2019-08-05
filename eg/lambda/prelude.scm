;; Prelude, built-ins

(to (build-prelude interpret apply)

  (to (make-church<-claim lc-no)
    {primitive (on (lc-yes)
                 {primitive (case
                              (#no  lc-no)
                              (#yes lc-yes))})})

  (to (make-church<-count lc-zero)
    {primitive
     (on (lc-succ)
       {primitive (on (n)
                    (begin counting ((n n))
                      (be n
                        (0          lc-zero)
                        ((? count?) (apply lc-succ (counting (- n 1)))))))})})

  (to (make-church<-list lc-nil)
    {primitive
     (on (lc-link)
       {primitive (on (xs)
                    (begin linking ((xs xs))
                      (be xs
                        ('()       lc-nil)
                        (`(,h ,@t) (apply (apply lc-link h) (linking t))))))})})

  (let builtins-env
    {module
     (map<- `(
              (make-church<-claim {primitive ,make-church<-claim})

              (add1 {primitive ,(on (n) (+ n 1))})
              (make-church<-count {primitive ,make-church<-count})

              (squeam-link {primitive ,(on (h)
                                         {primitive (on (t) (link h t))})})
              (make-church<-list {primitive ,make-church<-list})

              (display {primitive ,display})
              (error {primitive ,(to (lambda-error _)
                                   (error "Error in lambda-calculus program"))})
              ))})

  (let prelude
    '(do 

       (to (no if-yes if-no) if-no)
       (to (yes if-yes if-no) if-yes)
       (let church<-claim (make-church<-claim no yes))
       (to (claim<-church p) (p #yes #no))

       (to (if pred y n) (pred y n))
       (to (and p q) (if p q no))
       (to (or p q) (if p yes q))
       (to (not1 p) (p no yes)) ;; Right? Wikipedia sez "only correct for normal order". Why?
       (to (not p a b) (p b a))  ;; What they say is right for applicative order.

       (to (zero f x) x)
       (to (succ n f x) (f (n f x)))
       (let church<-count (make-church<-count zero succ))
       (to (count<-church n) (n add1 0))

       (to (+ m) (m succ))
       (to (* m n) (m (+ n) zero))
       (to (expt m n) (n m))

       (to (nil if-link if-nil) if-nil)
       (to (link h t if-link if-nil) (if-link h (t if-link if-nil)))
       (let church<-list (make-church<-list nil link))
       (to (list<-church xs) (xs squeam-link '()))

       (to (chain xs ys) (xs link ys))

       (to (compose f g x) (f (g x)))
       (to (identity x) x)

       (to (fix maker)
         (to (r recur) (maker ([x] (recur recur x))))
         (r r))

       (the-environment)))

  (interpret prelude builtins-env))

(export build-prelude)
