;; Basic PEG-ish parsing

;; TODO: track farthest reached
;; TODO: 'not' combinator
;; TODO: reasonable efficiency
;; TODO: memoize

(load "stdlib.scm")

(define (empty chars vals)
  (list<- chars vals))

(define (fail chars vals)
  #f)

(define (folded<- combine)
  (make
    ('run (p) p)
    ('run (p q) (combine p q))
    ('run (p q r) (combine p (combine q r)))
    ('run (p q r s) (combine p (combine q (combine r s))))
    ('run (p q r s t) (combine p (combine q (combine r (combine s t)))))
    ('run (p q r s t u) (combine p (combine q (combine r (combine s (combine t u))))))
    ))

(define either
  (folded<- (lambda (p q)
              (lambda (chars vals)
                (let ((result (p chars vals)))
                  (if result result (q chars vals)))))))

(define seq
  (folded<- (lambda (p q)
              (lambda (chars vals)
                (let ((result (p chars vals)))
                  (if result
                      (q (result 0) (result 1))
                      #f))))))

(define (feed-list f)
  (lambda (chars vals)
    (list<- chars (list<- (f vals)))))

(define (feed f)
  (feed-list (lambda (vals) (call 'run f vals))))

(define (seclude p)
  (lambda (chars vals)
    (let ((result (p chars '())))
      (if result
          (list<- (result 0) (chain vals (result 1)))
          #f))))

(define (eat1 ok?)
  (lambda (chars vals)
    (if ('empty? chars)
        #f
        (if (ok? ('first chars))
            (list<- ('rest chars) (chain vals (list<- ('first chars))))
            #f))))
    
(define (skip1 ok?)
  (lambda (chars vals)
    (if ('empty? chars)
        #f
        (if (ok? ('first chars))
            (list<- ('rest chars) vals)
            #f))))
    
(define any1 (eat1 (lambda (char) #t)))

(define (lit1 my-char)
  (skip1 (lambda (char) (is? my-char char))))

(define (maybe p)
  (either p empty))

(define (many p)
  (letrec ((p* (maybe (seq p (lambda (cs vs)
                               (p* cs vs))))))
    p*))


;; Smoke test

(print (any1 "a" '()))
(print ((seclude any1) "a" '()))
(print ((many any1) "abc" '()))

(define bal
  (lambda (cs vs)
    ((maybe (seq (lit1 #\() bal (lit1 #\)) bal))
     cs vs)))

(print (bal "(abc" '()))
(print (bal "()xyz" '()))
(print (bal "()()xyz" '()))
(print (bal "(()(()))" '()))

(print ((many (lit1 #\space)) "  hey" '()))


(define symbol<-chars (compose symbol<- string<-list))

(define hug (feed-list (lambda (vals) vals)))

(define sexpr
  (let ((subexpr (lambda (cs vs) (sexpr cs vs)))
        (_ (many (lit1 #\space))))
    (seclude
     (seq _ (either (seq (lit1 #\() _ (many subexpr) (lit1 #\)) _
                         hug)
                    (seq (eat1 'alphabetic?) (many (eat1 'alphanumeric?)) _
                         (feed-list symbol<-chars)))))))

(print (sexpr "" '()))
(print (sexpr "yo" '()))
(print (sexpr "(lisp)" '()))
(print (sexpr "(lisp (the  GREATEST  ) hurrah)" '()))
