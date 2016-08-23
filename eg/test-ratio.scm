(let ratio (use "lib/ratio.scm"))

(import ratio ratio<- as-float)
(let (r+ r- r* r/ r-compare) (each ratio '(+ - * / compare)))

;; Generate good rational approximations of float in order of
;; increasing denominator.
(define (rationalize float)
  (if (< float 0)
      (each/lazy - (rationalize (- float)))
      XXX))

(let pi (r/ (ratio<- 355) (ratio<- 113)))
(let tau (r+ pi pi))
(print pi)
(print tau)
(print (r/ tau pi))
(print (r- (ratio<- 1 2) (ratio<- 1 3)))
