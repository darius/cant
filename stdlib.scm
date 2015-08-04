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

(define list<-
  (make ('run () '())
        ('run (a) (cons a '()))
        ('run (a b) (cons a (list<- b)))
        ('run (a b c) (cons a (list<- b c)))
        ('run (a b c d) (cons a (list<- b c d)))))

(define chain
  (make ('run () '())
        ('run (xs) xs)
        ('run (xs ys) (foldr cons ys xs))
        ('run (xs ys zs) (chain xs (chain ys zs)))
        ('run (ws xs ys zs) (chain ws (chain xs ys zs)))))

(define (foldr f z xs)
  (if ('empty? xs)
      z
      (f ('first xs) (foldr f z ('rest xs)))))

(define (memq? x set)
  (if ('empty? set)
      #f
      (if (is? x ('first set))
          #t
          (memq? x ('rest set)))))

(define (list-index x xs)
  (letrec ((searching (lambda (i xs)
                        (if (is? x ('first xs))
                            i
                            (searching ('+ n 1) ('rest xs))))))
    (searching 0 xs)))

(define (print x)
  (write x)
  (newline))

(define (for-each f xs)
  (if ('empty? xs)
      #f
      (begin (f ('first xs))
             (for-each f ('rest xs)))))
