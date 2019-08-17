(let ratio (use 'ratio))

(import ratio ratio<- as-float)
(let `(,r+ ,r- ,r* ,r/ ,r-compare) (each ratio '(+ - * / compare)))

;; Generate good rational approximations of float in order of
;; increasing denominator.
(to (rationalize float)
  (if (< float 0)
      (each/lazy - (rationalize (- float)))
      (error "XXX unimplemented")))

(let pi (r/ (ratio<- 355) (ratio<- 113)))
(let tau (r+ pi pi))
(print pi)
(print (as-float pi))
(print tau)
(print (r/ tau pi))
(print (r- (ratio<- 1 2) (ratio<- 1 3)))
