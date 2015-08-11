;; Like https://github.com/darius/regexercise_solutions/blob/master/star.py
;; ~/git/regexercise_solutions/star.py
;; (using not-yet-implemented syntax)

;; N.B. the elements needn't be chars...

;; TODO: nicer looping?

(define (search re chars)
  (or (.nullable? re)
      (recurse scanning ((states (set<- re)) (chars chars))
        (and (not (.empty? chars))
             (hide
              (let char (.first chars))
              (let states (set<-sequence (for flatmap ((state states))
                                           (.after state char))))
              (or (any (map .nullable? states))
                  (scanning states (.rest chars))))))))

;; XXX also need comparison/hashing -- "case classes" to automate that?

(make empty
  (.nullable? () #y)
  (.after (char) '()))

(define (literal<- my-char)
  (make
    (.nullable? () #n)
    (.after (char) (if (is? char my-char) ; XXX need to think about equality
                       (list<- empty)
                       '()))))

(define (either<- r s)
  (make
    (.nullable? () (or (.nullable? r) (.nullable? s)))
    (.after (char) (chain (.after r char) (.after s char)))))

(define (chain<- r s)
  (if (is? r empty)
      s
      (make
        (.nullable? () (and (.nullable? r) (.nullable? s)))
        (.after (char)
          (let dr+s (for map ((r-rest (.after r char)))
                      (chain<- r-rest s)))
          (if (.nullable? r)
              (chain dr+s (.after s char))
              dr+s)))))

(define (star<- r)
  (make star
    (.nullable? () #y)
    (.after (char) (for map ((r-rest (.after r char)))
                     (chain<- r-rest star)))))
                        

;; So the Python came out a little shorter even with more comments and
;; no technicalities still to fill in.

;; TODO: work out what this would look like with "case classes"
;; or algebraic datatypes and matching.
