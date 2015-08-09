(define (starts-with? form tag)
  (and (pair? form) (eq? (car form) tag)))

;;TODO: make this something like ('coerce boolean? x)
(define (boolean<- x)                   
  (not (not x)))

(define (assert ok? plaint culprit)
  (if (not ok?)
      (error plaint culprit)))

(define (should= x expected)
  (assert (equal? x expected) "Expected" expected))

(define (identity x) x)
