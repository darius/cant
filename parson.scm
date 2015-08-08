;; Basic PEG-ish parsing

;; TODO: track farthest reached
;; TODO: 'not' combinator
;; TODO: reasonable efficiency
;; TODO: memoize

(load "stdlib.scm")
;(load "traceback.scm")

(define (empty chars vals)
  (make
    ('leftovers () chars)
    ('results () vals)
    ('continue (p) (p chars vals))
    ('prepend (pre-vals) (empty chars (chain pre-vals vals)))
    ))

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
                      ('continue result q)
                      #f))))))

(define (feed-list f)
  (lambda (chars vals)
    (empty chars (list<- (f vals)))))

(define (feed f)
  (feed-list (lambda (vals) (call 'run f vals))))

(define (seclude p)
  (lambda (chars vals)
    (let ((result (p chars '())))
      (if result
          ('prepend result vals)
          #f))))

(define (eat1 ok?)
  (lambda (chars vals)
    (if ('empty? chars)
        #f
        (if (ok? ('first chars))
            (empty ('rest chars) (chain vals (list<- ('first chars))))
            #f))))
    
(define (skip1 ok?)
  (lambda (chars vals)
    (if ('empty? chars)
        #f
        (if (ok? ('first chars))
            (empty ('rest chars) vals)
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

(define (try p string)
  (write string)
  (display " --> ")
  (let ((outcome (p string '())))
    (if outcome
        (begin
          (write ('leftovers outcome))
          (display " ")
          (print ('results outcome)))
        (print 'failed))))

(try any1 "a")
(try (seclude any1) "a")
(try (many any1) "abc")

(define bal
  (lambda (cs vs)
    ((maybe (seq (lit1 #\() bal (lit1 #\)) bal))
     cs vs)))

(try bal "(abc")
(try bal "()xyz")
(try bal "()()xyz")
(try bal "(()(()))")

(try (many (lit1 #\space)) "  hey")


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

(try sexpr "")
(try sexpr "yo")
(try sexpr "(lisp)")
(try sexpr "(lisp (the  GREATEST  ) hurrah)")
