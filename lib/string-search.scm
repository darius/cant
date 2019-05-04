;; Based on my old scheme-data-structures code
;; XXX testme

;; XXX this should be in runtime.scm somewhere
(let alphabet-size 256)


;;; String matching
;;; Return index of first substring in DAT equal to PAT, or #no.
;;; Note that (string-match "" "") = 0.

;; Brute force algorithm
;; XXX make this a built-in string method
(to (string-match/brute pat dat)
  (let P pat.count)
  (let j-limit (+ (- dat.count P) 1))
  (begin sliding ((j 0))
    (and (< j j-limit)
         (begin checking ((i 0))
           (case ((= i P)
                  j)
                 ((= (pat i) (dat (+ i j)))
                  (checking (+ i 1)))
                 (else
                  (sliding (+ j 1))))))))

;; XXX better name?
;; Boyer-Moore-Horspool algorithm
(to (string-matcher<- pat)
  (let m (- pat.count 1))
  (if (< m 0)
      (given (dat) 0)
      (do (let skip (array<-count alphabet-size pat.count))
          (for each! ((`(,i ,ch) pat.items))
            (skip .set! ch.code (- m i)))
          ;; On a mismatch whose last char of dat == (pat i), we'll
          ;; slide right by 1 + (|pat| - i). Then that last char will
          ;; align with (pat new-i). It'll be the rightmost (pat i)
          ;; with that char, because the above loop is ascending.

          ;; Search
          (given (dat)
            (let D dat.count)
            (begin sliding ((i m))
              (and (< i D)
                   (begin checking ((k i) (j m))
                     (case ((not= (dat k) (pat j)) ;NB this can go first because of (< m 0) above
                            (sliding (+ i (skip ((dat i) .code)))))
                           ((= j 0)
                            k)
                           (else
                            (checking (- k 1) (- j 1)))))))))))

(export string-matcher<-)
