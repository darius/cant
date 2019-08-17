;; Smoke test

(print (set? '(a b c)))
(print (set? '(a b c b)))

(print (append '(a b c) 'a))
(print (append '(a b c) 'd))

(print (bound? 'x '(a b c)))
(print (bound? 'x '(a x y)))
(print (bound? 'x 'any))

(print (expr? prelude 'any
              (parse-e '(car (if (+ a 2) (cdr x) (cdr (car x)))))))

(print (call-arity? prelude
                    (parse-e '(+ 2 3 4))))

(print (steps? prelude
               (parse-steps '((XXX-path (+ a b))))))
(print (steps? prelude
               (parse-steps '((XXX-path (+ 1 2))))))

(print (unparse-e (sub-e '(a)
                         (each parse-e '((< 2 3)))
                         (parse-e '(+ a 3)))))

(print (defs? prelude '()))

(print (bob/step prelude 
                 (parse-e '(car (cons 'ham '(eggs))))
                 (parse-steps '())))
(print (bob/step prelude 
                 (parse-e '(car (cons 'ham '(eggs))))
                 (parse-steps '(((1) (cons 'ham '(eggs)))
                                (() (car '(ham eggs)))))))

;; ch1/ex8
(print (bob/step prelude 
                 (parse-e '(car (cons (= (cons x y) (cons x y))
                                      '(and crumpets))))
                 (parse-steps '(((1 1) (equal-same (cons x y)))
                                ((1) (cons 't '(and crumpets)))
                                (() (car '(#yes and crumpets)))))))

(let defun-pair
  (J-Bob/define prelude
                '(((defun pair (x y)
                     (cons x (cons y '())))
                   #no))))
(print `(defun-pair ,defun-pair))

(let defun-first-of
  (J-Bob/define defun-pair
                '(((defun first-of (x)
                     (car x))
                   #no))))
(print defun-first-of)

(let defun-second-of
  (J-Bob/define defun-first-of
                '(((defun second-of (x)
                     (car (cdr x)))
                   #no))))
(print defun-second-of)

(let dethm-first-of-pair
  (J-Bob/define defun-second-of
                '(((dethm first-of-pair (a b)
                          (= (first-of (pair a b)) a))
                   #no
                   ((1 1) (pair a b))
                   ((1) (first-of (cons a (cons b '()))))
                   ((1) (car/cons a (cons b '())))
                   (() (equal-same a))))))
(pp (each unparse-def dethm-first-of-pair))

(let dethm-second-of-pair
  (J-Bob/define dethm-first-of-pair
                '(((dethm second-of-pair (a b)
                          (= (second-of (pair a b)) b))
                   #no
                   ((1) (second-of (pair a b)))
                   ((1 1 1) (pair a b))
                   ((1 1) (cdr/cons a (cons b '())))
                   ((1) (car/cons b '()))
                   (() (equal-same b))))))
(pp (each unparse-def dethm-second-of-pair))

(let defun-in-pair?
  (J-Bob/define dethm-second-of-pair
                '(((defun in-pair? (xs)
                     (if (= (first-of xs) '?) 't (= (second-of xs) '?)))
                   #no))))

(let dethm-in-first-of-pair
  (J-Bob/define defun-in-pair?
                '(((dethm in-first-of-pair (b)
                          (= (in-pair? (pair '? b)) 't))
                   #no
                   ((1 1) (pair '? b))
                   ((1) (in-pair? (cons '? (cons b '()))))
                   ((1 Q 1) (first-of (cons '? (cons b '()))))
                   ((1 Q 1) (car/cons '? (cons b '())))
                   ((1 Q) (equal-same '?))
                   ((1) (if-true 't (= (second-of (cons '? (cons b '()))) '?)))
                   (() (equal-same 't))))))

(let dethm-in-second-of-pair
  (J-Bob/define dethm-in-first-of-pair
                '(((dethm in-second-of-pair (a)
                          (= (in-pair? (pair a '?)) 't))
                   #no
                   ((1 1) (pair a '?))
                   ((1) (in-pair? (cons a (cons '? '()))))
                   ((1 Q 1) (first-of (cons a (cons '? '()))))
                   ((1 Q 1) (car/cons a (cons '? '())))
                   ((1 E 1) (second-of (cons a (cons '? '()))))
                   ((1 E 1 1) (cdr/cons a (cons '? '())))
                   ((1 E 1) (car/cons '? '()))
                   ((1 E) (equal-same '?))
                   ((1) (if-same (= a '?) 't))
                   (() (equal-same 't))))))
(pp (each unparse-def dethm-in-second-of-pair))
