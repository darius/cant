;; Basic PEG-ish parsing

;; TODO: reasonable efficiency
;; TODO: memoize

;(include "stdlib.scm")
(include "traceback.scm")

;; Glossary:
;;  p, q       parsing expression
;;  text       input sequence
;;  far        the rightmost index tentatively eaten up to in text
;;             (used for error reporting)
;;  i, j       index into text
;;  vals, vs   list of parsed values

(define (fail text far i vals)
  (make failure
    (.display ()
      (display "failed: ")
      (write (.slice text 0 far))
      (display "/")
      (write (.slice text far)))
    (.invert ()               empty)
    ;; XXX don't think we need far2
    (.else (p text far2 j vs) (p text (max far far2) j vs))
    (.continue (p)            failure)
    (.capture-from (j)        failure)
    (.prefix (pre-vals)       failure)
    (.leftovers ()            (error "Parsing failed" failure))
    (.opt-results ()          #f)
    (.result ()               (error "Parsing failed" failure))))

(define (empty text far i vals)
  (make success
    (.display () 
      (write (.slice text i))
      (display " ")
      (write vals))
    (.invert ()               fail)
    (.else (p text far2 j vs) success)
    (.continue (p)            (p text far i vals))
    (.capture-from (j)        (empty text far i `(,@vals ,(.slice text j i))))
    (.prefix (pre-vals)       (empty text far i (chain pre-vals vals)))
    (.leftovers ()            i)
    (.opt-results ()          vals)
    (.result ()
      (if (= 1 (.count vals))
          ('first vals)
          (error "Wrong # of results" vals)))))

(define (invert p)
  (given (text far i vals)
    ((.invert (p text far i vals))
     text far i vals)))

(define (capture p)
  (given (text far i vals)
    (.capture-from (p text far i vals) i)))

(define (folded<- combine)
  (given arguments
    (foldr1 combine arguments)))

(let either
  (folded<- (given (p q)
              (given (text far i vals)
                (.else (p text far i vals) q text far i vals)))))

(let seq
  (folded<- (given (p q)
              (given (text far i vals)
                (.continue (p text far i vals) q)))))

(define (feed-list f)
  (given (text far i vals)
    (empty text far i (list<- (f vals)))))

(define (feed f)
  (feed-list (given (vals) (call '.run f vals))))

(define (push constant)
  (given (text far i vals)
    (empty text far i `(,@vals ,constant))))

(define (seclude p)
  (given (text far i vals)
    (.prefix (p text far i '()) vals)))

(define (take-1 ok?)
  (capture (skip-1 ok?)))
    
(define (skip-1 ok?)
  (given (text far i vals)
    (if (and (.has? text i) (ok? (text i)))
        (empty text (max far (+ i 1)) (+ i 1) vals)
        (fail text far i vals))))

(let any-1 (take-1 (given (char) #t)))

(define (lit-1 my-char)
  (skip-1 (given (char) (is? my-char char))))

(define (maybe p)
  (either p empty))

(define (many p)
  (let p* (maybe (seq p (given (text far i vs)
                          (p* text far i vs)))))
  p*)


;; Smoke test

(define (try p text)
  (write text)
  (display " --> ")
  (.display (p text 0 0 '()))
  (newline))

(try any-1 "a")
(try (seclude any-1) "a")
(try (many any-1) "abc")

(let bal
  (given (text far i vs)
    ((maybe (seq (lit-1 #\() bal (lit-1 #\)) bal))
     text far i vs)))

(try bal "(abc")
(try bal "()xyz")
(try bal "()()xyz")
(try bal "(()(()))")

(try (many (lit-1 #\space)) "  hey")

(let hug (feed-list (given (vals) vals)))

(let sexpr
  (hide
   (let subexpr (given (text far i vs) (sexpr text far i vs)))
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
(try sexpr "(oops (unbalanced parens -- before unknown chars))")
