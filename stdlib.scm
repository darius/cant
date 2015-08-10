(define (union set1 set2)
  (let ((adjoin (lambda (x xs)
                  (if (memq? x set2) xs (cons x xs)))))
    (foldr adjoin set2 set1)))

(define (delq x set)
  (foldr (lambda (element rest)
           (if (is? x element) rest (cons element rest)))
         '()
         set))

(define (map f xs)
  (foldr (lambda (x ys) (cons (f x) ys))
         '()
         xs))

(define (foldr f z xs)
  (if ('empty? xs)
      z
      (f ('first xs) (foldr f z ('rest xs)))))

(define (foldr1 f xs)
  (let ((tail ('rest xs)))
    (if ('empty? tail)
        ('first xs)
        (f ('first xs) (foldr1 f tail)))))

(define list<-
  (make ('run () '())
        ('run (a) (cons a '()))
        ('run (a b) (cons a (list<- b)))
        ('run (a b c) (cons a (list<- b c)))
        ('run (a b c d) (cons a (list<- b c d)))
        (else (cue arguments)
          (if (is? cue 'run)
              (foldr1 cons arguments)
              (error "XXX need to punt to miranda methods" cue)))))

(define chain
  (make ('run () '())
        ('run (xs) xs)
        ('run (xs ys) ('chain xs ys))
        (else (cue arguments)
          (if (is? cue 'run)
              (foldr1 'chain arguments)
              (error "XXX need to punt to miranda methods" cue)))))

(define (memq? x set)
  (some (lambda (y) (is? x y)) set))

(define (some? ok? xs)
  (and (not ('empty? xs))
       (or (ok? ('first xs))
           (some? ok? ('rest xs)))))

(define (list-index xs x)
  (recurse searching ((i 0) (xs xs))
    (if (is? x ('first xs))
        i
        (searching ('+ n 1) ('rest xs)))))

(define (print x)
  (write x)
  (newline))

(define (for-each f xs)
  (when (not ('empty? xs))
    (f ('first xs))
    (for-each f ('rest xs))))

(define range<-
  (make
    ('run (hi-bound)
      (range<- 0 hi-bound))
    ('run (lo hi-bound)
      (if (<= hi-bound lo)
          '()
          (make ('empty? () #f)
                ('first () lo)
                ('rest () (range<- ('+ lo 1) hi-bound))
                ;; ...
                )))))

(define (vector<-list xs)
  (let ((v (vector<-count ('count xs))))
    (recurse setting ((i 0) (xs xs))
      (cond (('empty? xs) v)
            (else
             ('set! v i ('first xs))
             (setting ('+ i 1) ('rest xs)))))))

(define (compose f g)
  (lambda (x) (f (g x))))

;; XXX float contagion
(define (min x y) (if (< x y) x y))
(define (max x y) (if (< x y) y x))
