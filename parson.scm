;; Basic PEG-ish parsing

;; TODO: track farthest reached
;; TODO: 'not' combinator
;; TODO: reasonable efficiency
;; TODO: memoize

(load "stdlib.scm")
;(load "traceback.scm")

(define (empty chars vals)
  (make success
    ('display () 
      (write chars)
      (display " ")
      (write vals))
    ('result () (if (is? 1 ('count vals))
                    (vals 0)
                    (error "Wrong # of results" vals)))
    ('else (p cs vs) success)
    ('continue (p) (p chars vals))
    ('prefix (pre-vals) (empty chars (chain pre-vals vals)))
    ('leftovers () chars)
    ('results () vals)))

(define (fail chars vals)
  failure)

(define failure
  (make
    ('display () (display "failed"))
    ('else (p cs vs) (p cs vs))
    ('continue (p) failure)
    ('prefix (pre-vals) failure)))

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
                ('else (p chars vals) q chars vals)))))

(define seq
  (folded<- (lambda (p q)
              (lambda (chars vals)
                ('continue (p chars vals) q)))))

(define (feed-list f)
  (lambda (chars vals)
    (empty chars (list<- (f vals)))))

(define (feed f)
  (feed-list (lambda (vals) (call 'run f vals))))

(define (seclude p)
  (lambda (chars vals)
    ('prefix (p chars '()) vals)))

(define (take-1 ok?)
  (lambda (chars vals)
    (if ('empty? chars)
        failure
        (if (ok? ('first chars))
            (empty ('rest chars) (chain vals (list<- ('first chars))))
            failure))))
    
(define (skip-1 ok?)
  (lambda (chars vals)
    (if ('empty? chars)
        failure
        (if (ok? ('first chars))
            (empty ('rest chars) vals)
            failure))))

(define any-1 (take-1 (lambda (char) #t)))

(define (lit-1 my-char)
  (skip-1 (lambda (char) (is? my-char char))))

(define (maybe p)
  (either p empty))

(define (many p)
  (letrec ((p* (maybe (seq p (lambda (cs vs)
                               (p* cs vs))))))
    p*))


;; Smoke test

(define (try p string)
  (write string)
  (display " --> ")
  ('display (p string '()))
  (newline))

(try any-1 "a")
(try (seclude any-1) "a")
(try (many any-1) "abc")

(define bal
  (lambda (cs vs)
    ((maybe (seq (lit-1 #\() bal (lit-1 #\)) bal))
     cs vs)))

(try bal "(abc")
(try bal "()xyz")
(try bal "()()xyz")
(try bal "(()(()))")

(try (many (lit-1 #\space)) "  hey")


(define symbol<-chars (compose symbol<- string<-list))

(define hug (feed-list (lambda (vals) vals)))

(define sexpr
  (let ((subexpr (lambda (cs vs) (sexpr cs vs)))
        (_ (many (lit-1 #\space))))
    (seclude
     (seq _ (either (seq (lit-1 #\() _ (many subexpr) (lit-1 #\)) _
                         hug)
                    (seq (take-1 'alphabetic?) (many (take-1 'alphanumeric?)) _
                         (feed-list symbol<-chars)))))))

(try sexpr "")
(try sexpr "yo")
(try sexpr "(lisp)")
(try sexpr "(lisp (the  GREATEST  ) hurrah)")
