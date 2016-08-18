;; Complex numbers
;; TODO selfless objects so that complex numbers can have methods
;; TODO design automatic coercions with built-in real numbers

(define (conjugate {complex x y})
  {complex x (- y)})

(define (c-abs^2 {complex x y})
  (+ (* x x) (* y y)))

(define (c-abs c)
  (sqrt (c-abs^2 c)))

(define (r*c scale {complex x y})
  {complex (* scale x)
           (* scale y)})

(define (c+ {complex x1 y1} {complex x2 y2})
  {complex (+ x1 x2)
           (+ y1 y2)})

(define (c- {complex x1 y1} {complex x2 y2})
  {complex (- x1 x2)
           (- y1 y2)})

(define (c* {complex x1 y1} {complex x2 y2})
  {complex (- (* x1 x2) (* y1 y2))
           (+ (* x1 y2) (* y1 x2))})

(define (c/ c1 c2)
  (c* c1 (reciprocal c2)))

(define (reciprocal c)
  (r*c (/ 1 (c-abs^2 c)) (conjugate c)))

;; TODO fancier syntax for 'export' to handle this. Or just plain map literals.
(map<-a-list `(
               (conjugate ,conjugate)
               (abs^2 ,c-abs^2)
               (abs ,c-abs)
               (+ ,c+)
               (- ,c-)
               (* ,c*)
               (/ ,c/)
               ))
