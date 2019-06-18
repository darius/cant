;; Runtime.

(to (atom? x)   (not (link? x)))                      ;XXX right?
(to (bob-car x) (and (link? x) x.first))
(to (bob-cdr x) (and (link? x) x.rest))
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
    (cons (x y) ,link)
    (+ (x y)    ,bob+)
    (< (x y)    ,bob<)
    ))
