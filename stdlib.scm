(define (union set1 set2)
  (let ((adjoin (lambda (x xs)
                  (if (memq? x set2) xs (cons x xs)))))
    (foldr adjoin set2 set1)))

(define (delq x set)
  (if (eq? '() set)
      '()
      (if (eq? x ('car set))
          ('cdr set)
          (cons ('car set) (delq x ('cdr set))))))

(define (length xs)
  (letrec ((counting (lambda (n xs)
                       (if (eq? '() xs)
                           n
                           (counting ('+ n 1) ('cdr xs))))))
    (counting 0 xs)))

(define (map f xs)
  (foldr (lambda (x ys) (cons (f x) ys))
         '()
         xs))

(define (list1 x)          (cons x '()))
(define (append3 xs ys zs) (append2 xs (append2 ys zs)))
(define (append2 xs ys)    (foldr cons ys xs))

(define (foldr f z xs)
  (if (eq? '() xs)
      z
      (f ('car xs) (foldr f z ('cdr xs)))))

(define (memq? x set)
  (if (eq? '() set)
      #f
      (if (eq? x ('car set))
          #t
          (memq? x ('cdr set)))))

(define (list-index x xs)
  (letrec ((searching (lambda (i xs)
                        (if (eq? x ('car xs))
                            i
                            (searching ('+ n 1) ('cdr xs))))))
    (searching 0 xs)))
