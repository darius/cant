;; Rational numbers
;; same TODOs as complex numbers
;; n = numerator
;; d = denominator

;; N.B. all functions assume the {ratio n d} structures come in 
;; reduced to lowest terms (and with positive denom), and preserve
;; the same invariant.

(make ratio<-
  ((n)
   (assert (integer? n))
   {ratio n 1})
  ((n d)
   (assert (integer? n))
   (assert (integer? d))
   (reduce n d)))
    
(define (reduce n d)
  (case ((= d 0) (error "Divide by 0"))
        ((< d 0) (lowest-terms (- n) (- d)))
        (else    (lowest-terms n d))))

(define (lowest-terms n d)
  (let g (gcd n d))
  ;; TODO when we can mix number types: return an int if d/g = 1.
  {ratio (n .quotient g)
         (d .quotient g)})

(define (r* {ratio n1 d1} {ratio n2 d2})
  (lowest-terms (* n1 n2)
                (* d1 d2)))

(define (r/ {ratio n1 d1} {ratio n2 d2})
  (reduce (* n1 d2)
          (* d1 n2)))

(define (r+ {ratio n1 d1} {ratio n2 d2})
  (lowest-terms (+ (* n1 d2) (* n2 d1))
                (* d1 d2)))

(define (r- {ratio n1 d1} {ratio n2 d2})
  (lowest-terms (- (* n1 d2) (* n2 d1))
                (* d1 d2)))

(define (compare {ratio n1 d1} {ratio n2 d2})
  ((* n1 d2) .compare (* n2 d1)))

(define (as-float {ratio n d})
  (/ (exact->inexact n) d))

(map<-a-list `(
               (ratio<- ,ratio<-)
               (+ ,r+)
               (- ,r-)
               (* ,r*)
               (/ ,r/)
               (compare ,compare)
               (as-float ,as-float)
               ))
