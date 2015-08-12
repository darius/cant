;; Basic PEG-ish parsing

;; TODO: track farthest reached
;; TODO: reasonable efficiency
;; TODO: memoize

;(include "stdlib.scm")
(include "traceback.scm")

;; Glossary:
;;  p, q       parsing expression
;;  text       input sequence
;;  i, j       index into text
;;  vals, vs   list of parsed values

(define (fail text i vals)
  failure)

(make failure
  (.display ()         (display "failed"))
  (.invert ()          empty)
  (.else (p text j vs) (p text j vs))
  (.continue (p)       failure)
  (.capture-from (j)   failure)
  (.prefix (pre-vals)  failure))

(define (empty text i vals)
  (make success
    (.display () 
      (write (.slice text i))
      (display " ")
      (write vals))
    (.invert ()          fail)
    (.else (p text j vs) success)
    (.continue (p)       (p text i vals))
    (.capture-from (j)   (empty text i `(,@vals ,(.slice text j i))))
    (.prefix (pre-vals)  (empty text i (chain pre-vals vals)))
    (.leftovers ()       (.slice text i))
    (.results ()         vals)
    (.result ()
      (if (= 1 (.count vals))
          ('first vals)
          (error "Wrong # of results" vals)))))

(define (invert p)
  (given (text i vals)
    ((.invert (p text i vals))
     text i vals)))

(define (capture p)
  (given (text i vals)
    (.capture-from (p text i vals) i)))

(define (folded<- combine)
  (given arguments
    (foldr1 combine arguments)))

(let either
  (folded<- (given (p q)
              (given (text i vals)
                (.else (p text i vals) q text i vals)))))

(let seq
  (folded<- (given (p q)
              (given (text i vals)
                (.continue (p text i vals) q)))))

(define (feed-list f)
  (given (text i vals)
    (empty text i (list<- (f vals)))))

(define (feed f)
  (feed-list (given (vals) (call '.run f vals))))

(define (push constant)
  (given (text i vals)
    (empty text i `(,@vals ,constant))))

(define (seclude p)
  (given (text i vals)
    (.prefix (p text i '()) vals)))

(define (take-1 ok?)
  (capture (skip-1 ok?)))
    
(define (skip-1 ok?)
  (given (text i vals)
    (if (and (.has? text i) (ok? (text i)))
        (empty text (+ i 1) vals)
        failure)))

(let any-1 (take-1 (given (char) #t)))

(define (lit-1 my-char)
  (skip-1 (given (char) (is? my-char char))))

(define (maybe p)
  (either p empty))

(define (many p)
  (let p* (maybe (seq p (given (text i vs)
                          (p* text i vs)))))
  p*)


;; Smoke test

(define (try p string)
  (write string)
  (display " --> ")
  (.display (p string 0 '()))
  (newline))

(try any-1 "a")
(try (seclude any-1) "a")
(try (many any-1) "abc")

(let bal
  (given (text i vs)
    ((maybe (seq (lit-1 #\() bal (lit-1 #\)) bal))
     text i vs)))

(try bal "(abc")
(try bal "()xyz")
(try bal "()()xyz")
(try bal "(()(()))")

(try (many (lit-1 #\space)) "  hey")

(let hug (feed-list (given (vals) vals)))

(let sexpr
  (hide
   (let subexpr (given (text i vs) (sexpr text i vs)))
   (let __ (many (lit-1 #\space)))
   (seclude
    (seq __
         (either (seq (lit-1 #\() __ (many subexpr) (lit-1 #\)) __
                      hug)
                 (seq (take-1 '.alphabetic?) (many (take-1 '.alphanumeric?)) __
                      (feed chain) (feed symbol<-)))))))

(try sexpr "")
(try sexpr "yo")
(try sexpr "(lisp)")
(try sexpr "(lisp (the  GREATEST  ) hurrah)")
