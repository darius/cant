(define +
  (make
    (.run () 0)
    (.run (a) a)
    (.run (a b) (.+ a b))
    (.run arguments (foldr1 '.+ arguments))))

(define *
  (make
    (.run () 1)
    (.run (a) a)
    (.run (a b) (.* a b))
    (.run arguments (foldr1 '.* arguments))))

(define -
  (make
    (.run () (error "Bad arity"))
    (.run (a) (.- 0 a))
    (.run (a b) (.- a b))
    (.run arguments (foldl '.- (.first arguments) (.rest arguments)))))

(define (foldl f z xs)
  (if (.empty? xs)
      z
      ;;XXX is this the conventional arg order to f?
      (foldl f (f z (.first xs)) (.rest xs))))

(define (union set1 set2)
  (let ((adjoin (given (x xs)
                  (if (memq? x set2) xs (cons x xs)))))
    (foldr adjoin set2 set1)))

(define (delq x set)
  (foldr (given (element rest)
           (if (is? x element) rest (cons element rest)))
         '()
         set))

(define (map f xs)
  (foldr (given (x ys) (cons (f x) ys))
         '()
         xs))

(define (foldr f z xs)
  (if (.empty? xs)
      z
      (f (.first xs) (foldr f z (.rest xs)))))

(define (foldr1 f xs)
  (let ((tail (.rest xs)))
    (if (.empty? tail)
        (.first xs)
        (f (.first xs) (foldr1 f tail)))))

(define list<-
  (given arguments arguments))

(define chain
  (make (.run () '())
        (.run (xs) xs)
        (.run (xs ys) (.chain xs ys))
        (.run arguments (foldr1 '.chain arguments))))

(define (memq? x set)
  (some (given (y) (is? x y)) set))

(define (some? ok? xs)
  (and (not (.empty? xs))
       (or (ok? (.first xs))
           (some? ok? (.rest xs)))))

(define (list-index xs x)
  (recurse searching ((i 0) (xs xs))
    (if (is? x (.first xs))
        i
        (searching (+ n 1) (.rest xs)))))

(define (print x)
  (write x)
  (newline))

(define (each! f xs)
  (unless (.empty? xs)
    (f (.first xs))
    (each! f (.rest xs))))

(define range<-
  (make
    (.run (hi-bound)
      (range<- 0 hi-bound))
    (.run (lo hi-bound)
      (if (<= hi-bound lo)
          '()
          (make (.empty? () #f)
                (.first () lo)
                (.rest () (range<- (+ lo 1) hi-bound))
                ;; ...
                )))))

(define (vector<-list xs)
  (let ((v (vector<-count (.count xs))))
    (recurse setting ((i 0) (xs xs))
      (cond ((.empty? xs) v)
            (else
             (.set! v i (.first xs))
             (setting (+ i 1) (.rest xs)))))))

(define (compose f g)
  (given (x) (f (g x))))

;; XXX float contagion
(define (min x y) (if (< x y) x y))
(define (max x y) (if (< x y) y x))
