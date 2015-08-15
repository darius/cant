(define (cons car cdr)
  (make pair
    (.first () car)
    (.rest () cdr)))

;; In T:

(define-predicate pair?)
(define-settable-operation (car pair))
(define-settable-operation (cdr pair))

(define (cons the-car the-cdr)
  (object nil
    ((pair? self) t)
    ((car self) the-car)
    ((cdr self) the-cdr)
    (((setter car) self new-car) (set the-car new-car))
    (((setter cdr) self new-cdr) (set the-cdr new-cdr))))
