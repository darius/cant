;; Runtime.

(to (atom? x)   (not (cons? x)))                      ;XXX right?
(to (bob-car x) (and (cons? x) x.first))
(to (bob-cdr x) (and (cons? x) x.rest))
(to (nat? x)    (and (number? x) (integer? x) (<= 0 x)))
(to (bob+ x y)  (and (number? x) (number? y) (+ x y)))
(to (bob< x y)  (and (number? x) (number? y) (< x y)))

(to (size x)
  (match x
    (`(,h ,@t) (+ (size h) (size t)))
    (_ 1)))

(let x-ops
  `(
    (atom? (x)  ,atom?)
    (atom (x)   ,atom?)
    (car (x)    ,bob-car)
    (cdr (x)    ,bob-cdr)
    (nat? (x)   ,nat?)
    (natp (x)   ,nat?)
    (size (x)   ,size)
    (= (x y)    ,=)
    (cons (x y) ,cons)
    (+ (x y)    ,bob+)
    (< (x y)    ,bob<)
    ))


;; Prelude

(let x-axioms
  '(
    (dethm equal-same (x)
      (= (= x x) #yes))

    (dethm equal-swap (x y)
      (= (= x y) (= y x)))

    (dethm equal-if (x y)
      (if (= x y)
          (= x y)
          #yes))

    (dethm if-true (x y)
      (= (if #yes x y) x))

    (dethm if-false (x y)
      (= (if #no x y) y))

    (dethm if-same (x y)
      (= (if x y y) y))

    (dethm if-nest-A (x y z)
      (if x
          (= (if x y z) y)
          #yes))

    (dethm if-nest-E (x y z)
      (if x
          #yes
          (= (if x y z) z)))

    (dethm atom/cons (x y)
      (= (atom? (cons x y)) #no))

    (dethm car/cons (x y)
      (= (car (cons x y)) x))

    (dethm cdr/cons (x y)
      (= (cdr (cons x y)) y))

    (dethm cons/car+cdr (x)
      (if (atom? x)
          #yes
          (= (cons (car x) (cdr x)) x)))

    ;; TODO more
    ))

(let prelude
  ;; TODO more
  (each parse-def (chain x-ops x-axioms)))
