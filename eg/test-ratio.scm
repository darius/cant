(let ratio (use "lib/ratio.scm"))

(import ratio ratio<- as-float)
(let (r+ r- r* r/ r-compare) (each ratio '(+ - * / compare)))

;; Generate good rational approximations of float in order of
;; increasing denominator.
(define (rationalize float)
  (if (< float 0)
      (each/lazy - (rationalize (- float)))
      XXX))

(print (r/ (ratio<- 355) (ratio<- 113)))
