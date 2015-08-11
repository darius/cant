;; Basic PEG-ish parsing

;; TODO: track farthest reached
;; TODO: reasonable efficiency
;; TODO: memoize

(load "stdlib.scm")
;(load "traceback.scm")

(define (fail chars vals)
  failure)

(define failure
  (make
    (.display () (display "failed"))
    (.invert () empty)
    (.else (p cs vs) (p cs vs))
    (.continue (p) failure)
    (.capture (cs) failure)
    (.prefix (pre-vals) failure)))

(define (empty chars vals)
  (make success
    (.display () 
      (write chars)
      (display " ")
      (write vals))
    (.result () (if (is? 1 (.count vals))
                    (vals 0)
                    (error "Wrong # of results" vals)))
    (.invert () fail)
    (.else (p cs vs) success)
    (.continue (p) (p chars vals))
    (.capture (cs)
      ;; XXX this'd be simpler if we were working by indices already:
      (let ((d (.- (.count cs) (.count chars))))
        (empty chars `(,@vals ,(.slice cs 0 d)))))
    (.prefix (pre-vals) (empty chars (chain pre-vals vals)))
    (.leftovers () chars)
    (.results () vals)))

(define (invert p)
  (given (chars vals)
    ((.invert (p chars vals))
     chars vals)))

(define (capture p)
  (given (chars vals)
    (.capture (p chars vals) chars)))

(define (folded<- combine)
  (given arguments
    (foldr1 combine arguments)))

(define either
  (folded<- (given (p q)
              (given (chars vals)
                (.else (p chars vals) q chars vals)))))

(define seq
  (folded<- (given (p q)
              (given (chars vals)
                (.continue (p chars vals) q)))))

(define (feed-list f)
  (given (chars vals)
    (empty chars (list<- (f vals)))))

(define (feed f)
  (feed-list (given (vals) (call '.run f vals))))

(define (push constant)
  (given (chars vals)
    (empty chars `(,@vals ,constant))))

(define (seclude p)
  (given (chars vals)
    (.prefix (p chars '()) vals)))

(define (take-1 ok?)
  (capture (skip-1 ok?)))
    
(define (skip-1 ok?)
  (given (chars vals)
    (if (and (not (.empty? chars))
             (ok? (.first chars)))
        (empty (.rest chars) vals)
        failure)))

(define any-1 (take-1 (given (char) #t)))

(define (lit-1 my-char)
  (skip-1 (given (char) (is? my-char char))))

(define (maybe p)
  (either p empty))

(define (many p)
  (letrec ((p* (maybe (seq p (given (cs vs)
                               (p* cs vs))))))
    p*))


;; Smoke test

(define (try p string)
  (write string)
  (display " --> ")
  (.display (p string '()))
  (newline))

(try any-1 "a")
(try (seclude any-1) "a")
(try (many any-1) "abc")

(define bal
  (given (cs vs)
    ((maybe (seq (lit-1 #\() bal (lit-1 #\)) bal))
     cs vs)))

(try bal "(abc")
(try bal "()xyz")
(try bal "()()xyz")
(try bal "(()(()))")

(try (many (lit-1 #\space)) "  hey")

(define hug (feed-list (given (vals) vals)))

(define sexpr
  (let ((subexpr (given (cs vs) (sexpr cs vs)))
        (_ (many (lit-1 #\space))))
    (seclude
     (seq _ (either (seq (lit-1 #\() _ (many subexpr) (lit-1 #\)) _
                         hug)
                    (seq (take-1 '.alphabetic?) (many (take-1 '.alphanumeric?)) _
                         (feed chain) (feed symbol<-)))))))

(try sexpr "")
(try sexpr "yo")
(try sexpr "(lisp)")
(try sexpr "(lisp (the  GREATEST  ) hurrah)")
