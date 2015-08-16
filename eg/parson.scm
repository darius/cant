;; Basic PEG-ish parsing

;; TODO: fuller error reporting
;; TODO: memoize
;; TODO: delay semantic actions until final success

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
    (.invert ()          empty)
    (.else (p text j vs) (p text far j vs))
    (.continue (p)       failure)
    (.capture-from (j)   failure)
    (.prefix (pre-vals)  failure)
    (.leftovers ()       (error "Parsing failed" failure))
    (.opt-results ()     #f)
    (.result ()          (error "Parsing failed" failure))))

(define (empty text far i vals)
  (make success
    (.display () 
      (write (.slice text i))
      (display " ")
      (write vals))
    (.invert ()          fail)
    (.else (p text j vs) success)
    (.continue (p)       (p text far i vals))
    (.capture-from (j)   (empty text far i `(,@vals ,(.slice text j i))))
    (.prefix (pre-vals)  (empty text far i (chain pre-vals vals)))
    (.leftovers ()       i)
    (.opt-results ()     vals)
    (.result ()
      (if (= 1 (.count vals))
          ('first vals)
          (error "Wrong # of results" vals)))))

(define ((invert p) text far i vals)
  ((.invert (p text far i vals))
   text far i vals))

(define ((capture p) text far i vals)
  (.capture-from (p text far i vals) i))

(define (folded<- combine)
  (given arguments
    (foldr1 combine arguments)))

(let either
  (folded<- (given (p q)
              (given (text far i vals)
                (.else (p text far i vals) q text i vals)))))

(let then
  (folded<- (given (p q)
              (given (text far i vals)
                (.continue (p text far i vals) q)))))

(define ((feed-list f) text far i vals)
  (empty text far i `(,(f vals))))

(define (feed f)
  (feed-list (given (vals) (call '.run f vals))))

(define ((push constant) text far i vals)
  (empty text far i `(,@vals ,constant)))

(define ((seclude p) text far i vals)
  (.prefix (p text far i '()) vals))

;;TODO: implement promises instead
(define ((delay thunk) text far i vs)
  ((thunk) text far i vs))

(define ((skip-1 ok?) text far i vals)
  (if (and (.has? text i) (ok? (text i)))
      (empty text (max far (+ i 1)) (+ i 1) vals)
      (fail text far i vals)))


;; Derived combinators

(define (take-1 ok?)
  (capture (skip-1 ok?)))

(define ((always value) _)
  value)

(let any-1      (take-1 (always #t)))
(let skip-any-1 (skip-1 (always #t)))

(define (lit-1 my-char)
  (skip-1 (given (char) (is? my-char char))))

(define (maybe p)
  (either p empty))

(define (many p)
  (let p* (maybe (then p (delay (given () p*)))))
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

(let bal (hide
          (let sub-bal (delay (given () bal)))
          (maybe (then (lit-1 #\() sub-bal (lit-1 #\)) sub-bal))))

(try bal "(abc")
(try bal "()xyz")
(try bal "()()xyz")
(try bal "(()(()))")

(try (many (lit-1 #\space)) "  hey")

(let hug (feed-list (given (vals) vals)))

(let sexpr
  (hide
   (let subexpr (delay (given () sexpr)))
   (let comment (then (lit-1 #\;) (many (then (invert (lit-1 #\newline))
                                              skip-any-1))))
   (let __ (many (either (skip-1 '.whitespace?)
                         comment)))
   (seclude
    (then __
         (either (then (lit-1 #\() __ (many subexpr) (lit-1 #\)) __
                       hug)
                 (then (take-1 '.alphabetic?) (many (take-1 '.alphanumeric?)) __
                       (feed chain) (feed symbol<-)))))))

(try sexpr "")
(try sexpr "yo")
(try sexpr "(lisp)")
(try sexpr "(lisp (the  GREATEST  ) hurrah)")
(try sexpr "(oops (unbalanced parens -- before unknown chars))")
(try sexpr "(ok ; I am comment-goat.
hi)")
