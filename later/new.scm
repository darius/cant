;;; Trying out the new syntax scheme, with a little new semantics.

;;; pair.scm

(let list-trait
  (make-trait list
    ((i)
     (if (= i 0)
         list.first
         (list.rest (- i 1))))
    ({.empty?}
     (= 0 list.count)) ;N.B. these default implementations are circular
    ({.first}
     (list 0))
    ({.rest}
     (list .slice 1))
    ({.count}
     (if list.empty?
         0
         (+ 1 list.rest.count)))
    ({.slice i}
     (assert (<= i 0))
     (if (= i 0)
         list
         (list.rest .slice (- i 1))))
    ({.slice i bound}     ;XXX result is a cons-list; be more generic?
     (assert (<= i 0))
     (cond (list.empty? list)
           ((<= bound i) '())
           ((= i 0) (cons list.first (list.rest .slice 0 (- bound 1))))
           (else (list.rest .slice 0 (- bound 1)))))
    ({.chain seq}
     (if list.empty?
         seq
         (cons list.first (list.rest .chain seq))))
    ;; A sequence is a kind of collection. Start implementing that:
    ({.maps? key}
     (and (not list.empty?)
          (or (= 0 key)
              (and (< 0 key)
                   (list.rest .maps? (- key 1))))))
    ({.maps-to? value}
     (for some ((x list)) (= x value)))
    ({.find-key-for value}                  ;XXX name?
     (cond (list.empty? (error "Missing key" value))
           ((= value list.first) 0)
           (else (+ 1 (list.rest .find-key-for value)))))
    ;;...
    ))

(let (pair? pair-stamp) (stamp<- "pair"))

(define (cons head tail)
  ;; XXX where do we define the selflessness and equality?
  ;;     also, the pattern-matching syntax?
  (make pair
    stamped pair-stamp
    extending list-trait                ;XXX syntax?
    ({.empty?} #no)
    ({.first}  head)
    ({.rest}   tail)))


;;; stdlib.scm

(define (assert ok? plaint irritant)
  (unless ok?
    (error plaint irritant)))

(make +
  (() 0)
  ((a) a)
  ((a b) (a .+ b))
  ((& arguments)                        ;XXX syntax?
   (foldr1 '.+ arguments)))

(make *
  (() 1)
  ((a) a)
  ((a b) (a .* b))
  ((& arguments)
   (foldr1 '.* arguments)))

(make -
  (() (error "Bad arity"))
  ((a) (0 .- a))
  ((a b) (a .- b))
  ((& arguments)
   (foldl '.- arguments.first arguments.rest)))

;;XXX so shouldn't some of these be in list-trait?

(define (foldl f z xs)
  (if xs.empty?
      z
      (foldl f (f z xs.first) xs.rest))) ;XXX conventional arg order to f?

(define (union set1 set2)
  (define (adjoin x xs)
    (if (set2 .maps-to? x) xs (cons x xs)))
  (foldr adjoin set2 set1))

(define (remove set x)
  ;; XXX removes *all* instances -- but we know a set has at most 1
  (foldr (given (element rest)
           (if (= x element) rest (cons element rest)))
         '()
         set))

(define (each f xs)
  (foldr (given (x ys) (cons (f x) ys))
         '()
         xs))

(define (each-chained f xs)
  (foldr (given (x ys) (chain (f x) ys))
         '()
         xs))

(define (filter ok? xs)
  (foldr (given (x ys)
           (if (ok? x) (cons x ys) ys))
         '()
         xs))

(define (foldr f z xs)
  (if xs.empty?
      z
      (f xs.first (foldr f z xs.rest))))

(define (foldr1 f xs)
  (let tail xs.rest)
  (if tail.empty?
      xs.first
      (f xs.first (foldr1 f tail))))

(define (list<- & arguments)
  arguments)

(make chain
  (() '())
  ((xs) xs)
  ((xs ys) (xs .chain ys))
  ((& arguments) (foldr1 '.chain arguments)))

(define (some ok? xs)
  (and (not xs.empty?)
       (or (ok? xs.first)
           (some ok? xs.rest))))

(define (print x)
  (write x)
  (newline))

(define (each! f xs)
  (unless xs.empty?
    (f xs.first)
    (each! f xs.rest)))

(make range<-
  ((limit)
   (range<- 0 limit))
  ((first limit)
   (if (<= limit first)
       '()
       (make range extending list-trait
         ({.empty?} #no)
         ({.first}  first)
         ({.rest}   (range<- (+ first 1) limit))
         ({.count}  (- limit first))
         ((i)
          (let j (+ first i))
          (if (and (<= first j) (< j limit)) ;XXX also, integer?
              j
              (error "Out of range" range i)))
         ({.maps? i}
          (let j (+ first i))
          (and (<= first j) (< j limit))) ;XXX also, integer?
          ;; ...
          ))))

(define (vector<-list xs)
  (let v (vector<-count xs.count))
  (recurse setting ((i 0) (xs xs))
    (cond (xs.empty? v)
          (else
           (v .set! i xs.first)
           (setting (+ i 1) xs.rest)))))

(define (compose f g)
  (given (& arguments)
    (f (apply g arguments))))

;; XXX float contagion
(define (min x y) (if (< x y) x y))
(define (max x y) (if (< x y) y x))


;; traceback.scm
;; Install an error handler that prints a (crude) traceback.

(include "stdlib.scm")

(define (on-error-traceback k plaint values)
  (print-plaint plaint values)
  (print-traceback k))

(define (print-plaint plaint values)
  (display "Error! ")
  (write plaint)
  (display ": ")
  (write values)
  (newline))

(define (print-traceback k)
  (each! print k))

(the-signal-handler-box .set! on-error-traceback)


;; eg/circuitoptimizer.scm
;; From https://github.com/darius/superbench
;; ~/git/superbench/superopt/circuitoptimizer.scm

(define (superopt truth-table max-gates)
  (let n-inputs (int-log2 truth-table.count))
  (find-circuits (truth-table .parse-int 2) n-inputs max-gates))

(define (int-log2 n)
  ;; XXX ugly
  (if (= n 1) 0
      (if (= n 2) 1
          (if (= n 4) 2
              (if (= n 8) 3
                  (if (= n 16) 4
                      (if (= n 32) 5
                          (error "Bad argument" n))))))))

(define (say & arguments)
  (each! display arguments))

(define (pow2 n)
  (1 .<< n))

(define (find-circuits wanted n-inputs max-gates)
  (let inputs (vector<-list (tabulate-inputs n-inputs)))
  (let mask (- (pow2 (pow2 n-inputs)) 1))

  (define (print-formula L-input R-input)
    (let v-name (chain ("ABCDEF" .slice 0 n-inputs)
                       ("abcdefghijklmnopqrstuvwxyz" .slice n-inputs)))
    (for each! ((i (range<- L-input.count)))
      (let g (v-name (+ i n-inputs)))
      (let L (v-name (L-input i)))
      (let R (v-name (R-input i)))
      (say g " = " L " ~& " R "; "))
    (newline))

  (define (find-for-n n-gates)
    (say "Trying " n-gates " gates..." #\newline)
    (let n-wires (+ n-inputs n-gates))
    (let L-input (vector<-count n-gates #no))
    (let R-input (vector<-count n-gates #no))
    (let found?  (box<- #no))
    (let wire    (chain inputs L-input))
    (recurse sweeping ((gate 0))
      (for each! ((L (range<- (+ n-inputs gate))))
        (let L-wire (wire L))
        (L-input .set! gate L)          ;XXX how about .:= or .<- or something?
        (for each! ((R (range<- (+ L 1))))
          (let value (nand L-wire (wire R)))
          (R-input .set! gate R)
          (wire .set! (+ n-inputs gate) value)
          (cond ((< (+ gate 1) n-gates)
                 (sweeping (+ gate 1)))
                ((= wanted (mask .and value))
                 (found? .set! #yes)
                 (print-formula L-input R-input))))))
    (found?))

  (some find-for-n (range<- 1 (+ max-gates 1))))

(define (nand x y)
  ((x .and y) .not))     ;XXX why bitwise not here? only 1 bit, right?

(define (tabulate-inputs n-inputs)
  ;; An inputs vector is a vector of n-inputs bitvectors. It holds all
  ;; possible input patterns 'transposed': that is, the kth test case
  ;; can be formed out of bit #k of each the list's elements, one
  ;; element per circuit input. Transposed is the most useful form
  ;; because we can compute all test cases in parallel with bitwise
  ;; operators.
  (if (= n-inputs 0)
      '()
      (hide
       (let shift (pow2 (- n-inputs 1)))
       (cons (- (pow2 shift) 1)
             (for each ((iv (tabulate-inputs (- n-inputs 1))))
               (iv .or (iv .<< shift)))))))


(superopt "0110" 3)
(superopt "1011" 3)
;(superopt "0110" 4)


;; eg/compact-lambda.scm
(define (compile lexp)
  ((parse lexp) .compile '(HALT)))

(define (parse lexp)
  (cond ((symbol? lexp)
         (var-ref<- lexp))
        ((= (lexp 0) 'lambda)
         (abstraction<- ((lexp 1) 0)
                        (parse (lexp 2))))
        (else
         (call<- (parse (lexp 0))
                 (parse (lexp 1))))))

;; Variable reference
(define (var-ref<- v)
  (make
    ({.compile k} `(VAR ,v ,@k))))

;; Lambda expression
(define (abstraction<- v body)
  (make
    ({.compile k}
     (let code (body .compile '(RET)))
     `(LAM ,v ,code.count ,@code ,@k))))

;; Application
(define (call<- operator operand)
  (make
    ({.compile k}
     (let code (operator .compile (operand .compile '(call))))
     (if (= k.first 'RET)
         code
         `(SAVE ,code.count ,@code ,@k)))))


;; Smoke test

(print (compile
;  '(lambda (x) x)
;  '(lambda (x) y)
  '((lambda (x) (lambda (y) x)) (lambda (z) z))
))


;; eg/failing.scm
;; An example failing computation, to exercise the debugger.

(define (factorial n)
  (if (= 0 n)
      one
      (* n (factorial (- n 1)))))

(print (factorial 5))


;; eg/intset.scm
;; An example from William Cook's essay on OOP vs. ADTs
;; http://www.cs.utexas.edu/~wcook/Drafts/2009/essay.pdf

(make empty
  ({.empty?}   #yes)
  ({.has? k}   #no)
  ({.adjoin k} (adjoin<- k empty))
  ({.merge s}  s))

(define (adjoin<- n s)
  (if (s .has? n)
      s
      (make extension
        ({.empty?}   #no)
        ({.has? k}   (or (= n k) (s .has? k)))
        ({.adjoin k} (adjoin<- k extension))
        ({.merge s}  (merge<- extension s)))))

(define (merge<- s1 s2)
  (make meld
    ({.empty?}   (and s1.empty? s2.empty?))
    ({.has? k}   (or (s1 .has? k) (s2 .has? k)))
    ({.adjoin k} (adjoin<- k meld))
    ({.merge s}  (merge<- meld s))))

;; Smoke test

(let eg ((empty .adjoin 6) .adjoin 5))

(print (eg .has? 5))
(print (eg .has? 6))
(print (eg .has? 7))


;; eg/lambdacompiler.scm

(define (compile lexp)
  ((parse lexp) .compile global-static-env '(halt)))

(define (parse lexp)
  (case lexp
    ((: _ symbol?)
     (var-ref<- lexp))
    (('lambda (v) body)
     (abstraction<- v (parse body)))
    ((operator operand)
     (call<- (parse operator)
             (parse operand)))))

;; Variable reference
(define (var-ref<- v)
  (make ({.free-vars} (list<- v))
        ({.compile s k} (cons (s v) k))))

;; Lambda expression
(define (abstraction<- v body)
  (let free-vars (remove body.free-vars v))
  (make ({.free-vars} free-vars)
        ({.compile s k}
         (let code (body .compile (static-env<- v free-vars) '(return)))
         `(make-closure
           ,free-vars.count ,code.count ,@(each s free-vars)
           ,@code ,@k))))

;; Application
(define (call<- operator operand)
  (make ({.free-vars} (union operator.free-vars operand.free-vars))
        ({.compile s k}
         (let code (operand .compile s (operator .compile s '(invoke))))
         (if (= k.first 'return)
             code
             `(pushcont ,code.count ,@code ,@k)))))


;; Static environments (called 's' above)

(define (global-static-env v)
  (error "Unbound variable" v))

(define ((static-env<- param free-vars) v)
  (if (= v param)
      'local
      (+ 1 (free-vars .find-key-for v))))


;; Smoke test

(print (compile
;  '(lambda (x) x)
;  '(lambda (x) y)
  '((lambda (x) (lambda (y) x)) (lambda (z) z))  ; XXX is output wrong?
))


;; eg/lambdaterp.scm
;; Let's work out a source-level debugger in a simpler setting,
;; the call-by-value lambda calculus.
;; (That's the goal; not there yet.)

;; Conventions:
;;  lexp    source form of lambda-calculus expression
;;  c       constant value
;;  v       variable name (a symbol)
;;  r       environment
;;  k       continuation
;;  others  an AST or a value

(define (parse lexp)
  (cond ((symbol? lexp)
         (var-ref<- lexp))
        ((number? lexp)
         (constant<- lexp))
        ((= (lexp 0) 'lambda)
         (abstraction<- ((lexp 1) 0)
                        (parse (lexp 2))))
        (else
         (call<- (parse (lexp 0))
                 (parse (lexp 1))))))

(define (interpret lexp)
  ((parse lexp) .evaluate global-env halt))


;; ASTs and continuations

(make halt
  ({.empty?} #yes)
  ({.inject k<-} halt)
  ({.take-step val} val)
  ({.take val} val))


;; Constant
(define (constant<- c)
  (make constant
    ({.source} c)
    ({.eval-step r k} (debugging (value-step<- constant r k)))
    ({.evaluate r k} (k .take c))))

;; Variable reference
(define (var-ref<- v)
  (make var-ref
    ({.source} v)
    ({.eval-step r k} (debugging (value-step<- var-ref r k)))
    ({.evaluate r k} (lookup r v k))))

;; Lambda expression
(define (abstraction<- v body)
  (make abstraction
    ({.source} `(& ,v ,body.source))
    ({.eval-step r k} (debugging (value-step<- abstraction r k)))
    ({.evaluate r k}
     (k .take (make
                ({.survey} `(,v -> ...))
                ({.call arg k2}
                 (body .evaluate (extend r v arg) k2))
                ({.call-step arg k2}
                 (body .eval-step (extend r v arg) k2)))))))

;; Application
(define (call<- operator operand)
  (make app
    ({.source} `(,operator.source ,operand.source))
    ({.eval-step r k}
     (debugging (subeval-step<- operator r (ev-arg-cont<- operand r k))))
    ({.evaluate r k}
     (operator .evaluate r (ev-arg-cont<- operand r k)))))

(define (ev-arg-cont<- operand r k)
  (make ({.empty?} #no)
        ({.rest} k)
        ({.first} `(^ ,operand.source))
        ({.inject k<-} (ev-arg-cont<- operand r (k<- k)))
        ({.take fn}
         (operand .evaluate r (call-cont<- fn k)))
        ({.take-step fn}
         (operand .eval-step r (call-cont<- fn k)))
        ))

(define (call-cont<- fn k)
  (make ({.empty?} #no)
        ({.rest} k)
        ({.first} `(,(survey fn) ^))
        ({.inject k<-} (call-cont<- fn (k<- k)))
        ({.take arg}
         (fn .call arg k))
        ({.take-step arg}
         (fn .call-step arg k))
        ))


;; Built-in values

(define (survey value)
  (if (or (number? value) (symbol? value))
      value
      value.survey))

(make prim+
  ({.survey} '+)
  ({.call-step arg1 k1}
   XXX)
  ({.call arg1 k1}
   (if (number? arg1)
       (k1 .take (make
                   ({.survey} `(+ ,(survey arg1)))
                   ({.call-step arg2 k2}
                    XXX)
                   ({.call arg2 k2}
                    (if (number? arg2)
                        (k2 .take (+ arg1 arg2))
                        ;; XXX should supply self, too:
                        (debug k2 "Bad arg2 to +" (survey arg2))))))
       (debug k1 "Bad arg1 to +" (survey arg1)))))


;; Environments

(let global-env
  `((+ ,prim+)))

(define (extend r v val)
  `((,v ,val) ,@r))

(define (lookup r v k)
  (cond ((assq v r) => (given (record) (k .take (record 1))))
        (else (debug k "Unbound var" v))))


;; Debugger
;; Instead of interacting at a prompt, it takes a list of commands,
;; for now, for ease of rerunning during development.

(let command-queue (box<- '()))

(define (next-command)
  (display "debug> ")
  (let cmds (command-queue))
  (cond (cmds.empty?
         (newline)
         #no)
        (else
         (print cmds.first)
         (command-queue .set! cmds.rest)
         cmds.first)))

(define (debug k plaint irritant)
  (complain plaint irritant)
  (traceback k)
  (debugging (out-step<- k 'default-error-value)))

(define (complain plaint irritant)
  (display "Lambdaterp error: ")
  (write plaint)
  (display ": ")
  (write irritant)
  (newline))

(define (traceback k)
  (each! print k))

(define (debugging state)
  (let cmd (next-command))
  (if cmd (call state cmd) #no))

(define (value-step<- e r k)
  (make value-step
    ({.show}
     (display "ev-> ") (print e.source))
    ({.b}
     (traceback k)
     (debugging value-step))
    ({.continue}
     (e .evaluate r k))
    ({.hop}
     (e .evaluate r (k .inject debugger-trap-cont<-)))
    ({.step}
     value-step.hop)
    ))

(define (subeval-step<- e r k)
  (make subeval-step
    ({.show}
     (display "ev-> ") (print e.source))
    ({.b}
     (traceback k)
     (debugging subeval-step))
    ({.continue}
     (e .evaluate r k))
    ({.hop}
     (e .evaluate r (k .inject debugger-trap-cont<-)))
    ({.step}
     (e .eval-step r k))
    ))

(define (out-step<- k value)
  (make out-step
    ({.show}
     (display "<-ret ") (print (survey value)))
    ({.b}
     (traceback k)
     (debugging out-step))
    ({.continue}
     (k .take value))
    ({.hop}
     ((k .inject debugger-trap-cont<-) .take value))
    ({.step} 
     (k .take-step value))
    ({.value new-value}
     (debugging (out-step<- k new-value)))
    ))

(define (debugger-trap-cont<- k)
  (if (= k halt)
      k
      (make
        ({.take (value) (debugging (out-step<- k value)))
        ;; XXX (.inject ...) ?
        (else (cue arguments) (call cue k arguments)))))


;; Smoke test

(make try
  ((lexp)
   (try lexp '()))
  ((lexp commands)
   (command-queue .set! commands)
   (let result (interpret lexp))
   (if result (print (survey result)) 'failed)))

(try '(lambda (x) x))
(try '((lambda (x) ((+ x) 2)) 1))

(try '((lambda (x) ((+ y) 1)) 42))
(try '((+ (lambda (z) z)) y))
(try '(((+ 1) y) 2))

(try '((lambda (x) ((+ y) 1)) 42)
     '({.value 42} {.continue}))
(try '((lambda (x) ((+ y) 1)) 42)
     '({.value 42} {.hop} {.b} {.hop}))


;; eg/parse.scm
;; Ported from PAIP chapter 9

;; Return all complete parses of a list of words.
(define (parser words)
  (each '.tree (filter '.complete? (parse words))))

;; Return all parses of any prefix of words (working bottom-up).
(define (parse words)
  (if words.empty?
      '()
      (for each-chained ((rule (lexical-rules words.first)))
        (extend-parse rule.lhs `(,words.first) words.rest '()))))

;; Look for the categories needed to complete the parse.
(define (extend-parse lhs rhs rest needed)
  (cond (needed.empty?
         ;; Return parse and upward extensions.
         (let tree (tree<- lhs rhs))
         (cons (parse<- tree rest)
               (for each-chained ((rule (rules-starting-with lhs)))
                 (extend-parse rule.lhs `(,tree)
                               rest rule.rhs.rest))))
        (else
         ;; Try to extend rightward.
         (for each-chained ((p (parse rest)))
           (if (= p.lhs needed.first)
               (extend-parse lhs `(,@rhs ,p.tree)
                             p.remainder needed.rest)
               '())))))

(define (tree<- lhs rhs)
  (make
    ({.show} `(,lhs ,@(for each ((part rhs))
                        (if (symbol? part) part part.show))))
    ({.lhs} lhs)
    ({.rhs} rhs)))

(define (rule<- lhs rhs)
  (make
    ({.lhs} lhs)
    ({.rhs} rhs)
    ({.starts-with? cat}
     (and (list? rhs)
          (not rhs.empty?)
          (= rhs.first cat)))))

;; A parse tree and a remainder.
(define (parse<- tree remainder)
  (make
    ({.show}      `((tree: ,tree.show)
                    (remainder: ,remainder)))
    ({.tree}      tree)
    ({.remainder} remainder)
    ({.lhs}       tree.lhs)
    ({.complete?} remainder.empty?)))

;; Return a list of those rules with word on the rhs.
(define (lexical-rules word)
  (for filter ((rule (*grammar*)))
    (= rule.rhs word)))

;; Return a list of those rules where cat starts the rhs.
(define (rules-starting-with cat)
  (for filter ((rule (*grammar*)))
    (rule .starts-with? cat)))

(let *grammar* (box<- '()))

(define (grammar<- sexprs)
  (for each ((s sexprs))
    (assert (= (s 1) '->) "Bad rule syntax" s)
    (assert (= s.count 3) "Bad rule syntax" s)
    (rule<- (s 0) (s 2))))


;; Example grammars

(let grammar3
  (grammar<-
   '((Sentence -> (NP VP))
     (NP -> (Art Noun))
     (VP -> (Verb NP))
     (Art -> the) (Art -> a)
     (Noun -> man) (Noun -> ball) (Noun -> woman) (Noun -> table)
     (Noun -> noun) (Noun -> verb)
     (Verb -> hit) (Verb -> took) (Verb -> saw) (Verb -> liked))))

(let grammar4
  (grammar<-
   '((S -> (NP VP))
     (NP -> (D N))
     (NP -> (D A+ N))
     (NP -> (NP PP))
     (NP -> (Pro))
     (NP -> (Name))
     (VP -> (V NP))
     (VP -> (V))
     (VP -> (VP PP))
     (PP -> (P NP))
     (A+ -> (A))
     (A+ -> (A A+))
     (Pro -> I) (Pro -> you) (Pro -> he) (Pro -> she)
     (Pro -> it) (Pro -> me) (Pro -> him) (Pro -> her)
     (Name -> John) (Name -> Mary)
     (A -> big) (A -> little) (A -> old) (A -> young)
     (A -> blue) (A -> green) (A -> orange) (A -> perspicuous)
     (D -> the) (D -> a) (D -> an)
     (N -> man) (N -> ball) (N -> woman) (N -> table) (N -> orange)
     (N -> saw) (N -> saws) (N -> noun) (N -> verb)
     (P -> with) (P -> for) (P -> at) (P -> on) (P -> by) (P -> of) (P -> in)
     (V -> hit) (V -> took) (V -> saw) (V -> liked) (V -> saws))))


;; Smoke test

(define (try sentence)
  (write sentence) (display ":") (newline)
  (each! print (each '.show (parser sentence)))
  (newline))

(*grammar* .set! grammar3)

(try '(the table))
(try '(the ball hit the table))
(try '(the noun took the verb))

(*grammar* .set! grammar4)

;(try '(the man hit the table with the ball))
;(try '(the orange saw))


;; eg/parson.scm
;; PEG parsing

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
    ({.display}
      (display "failed: ")
      (write (text .slice 0 far))
      (display "/")
      (write (text .slice far)))
    ({.invert}           empty)
    ({.else p text j vs} (p text far j vs))
    ({.continue p}       failure)
    ({.capture-from j}   failure)
    ({.prefix pre-vals}  failure)
    ({.leftovers}        (error "Parsing failed" failure))
    ({.opt-results}      #no)
    ({.result}           (error "Parsing failed" failure))))

(define (empty text far i vals)
  (make success
    ({.display} 
      (write (text .slice i))
      (display " ")
      (write vals))
    ({.invert}           fail)
    ({.else p text j vs} success)
    ({.continue p}       (p text far i vals))
    ({.capture-from j}   (empty text far i `(,@vals ,(text .slice j i))))
    ({.prefix pre-vals}  (empty text far i (chain pre-vals vals)))
    ({.leftovers}        i)
    ({.opt-results}      vals)
    ({.result}
      (if (= 1 vals.count)
          vals.first
          (error "Wrong # of results" vals)))))

(define ((invert p) text far i vals)
  (let p-result (p text far i vals))
  (p-result.invert text far i vals))

(define ((capture p) text far i vals)
  (let p-result (p text far i vals))
  (p-result .capture-from i))

(define (folded<- combine)
  (given (& arguments)
    (foldr1 combine arguments)))

(let either
  (folded<- (define ((either p q) text far i vals)
              ((p text far i vals) .else q text i vals))))

(let then
  (folded<- (define ((then p q) text far i vals)
              ((p text far i vals) .continue q))))

(define ((feed-list f) text far i vals)
  (empty text far i `(,(f vals))))

(define (feed f)
  (feed-list (given (vals) (apply f vals))))

(define ((push constant) text far i vals)
  (empty text far i `(,@vals ,constant)))

(define ((seclude p) text far i vals)
  ((p text far i '()) .prefix vals))

;;TODO: implement promises instead
(define ((delay thunk) text far i vals)
  ((thunk) text far i vals))

(define ((skip-1 ok?) text far i vals)
  (if (and (text .maps? i) (ok? (text i)))
      (empty text (max far (+ i 1)) (+ i 1) vals)
      (fail text far i vals)))


;; Derived combinators

(define (take-1 ok?)
  (capture (skip-1 ok?)))

(define ((always value) _)
  value)

(let any-1      (take-1 (always #yes)))
(let skip-any-1 (skip-1 (always #yes)))

(define (lit-1 my-char)
  (skip-1 (given (char) (= my-char char))))

(define (maybe p)
  (either p empty))

(define (many p)
  (let p* (maybe (then p (delay (given () p*))))))


;; Smoke test

(define (try p text)
  (write text)
  (display " --> ")
  ((p text 0 0 '()) .display)
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
                       (feed (compose symbol<- chain))))))))

(try sexpr "")
(try sexpr "yo")
(try sexpr "(lisp)")
(try sexpr "(lisp (the  GREATEST  ) hurrah)")
(try sexpr "(oops (unbalanced parens -- before unknown chars))")
(try sexpr "(ok ; I am comment-goat.
hi)")


;; bdd.scm
;; from ~/git/mccarthy-to-bryant/lua/bdd3.lua

(let lit0 0)
(let lit1 1)

(define (constant<- value)
  (assert (or (= value lit0) (= value lit1))
          "Not binary" value)
  value)

(let infinite-rank #x7fffffff)
(let ranks (fillvector<- infinite-rank infinite-rank))
(let if0s  (fillvector<- lit0 lit1))
(let if1s  (fillvector<- lit0 lit1))
(let ifs   (vector<- if0s if1s))

(define (dedup memo k1 k2 k3)
  (define (enter map key)
    (let t (map .get key))
    (or t (hide (let v (map<-))
                (map .set! key v)
                v)))
  (let mem1 (enter memo k1))
  (let mem2 (enter mem1 k2))
  (list<- (mem2 .get k3) mem2))

(let choice-memo (map<-))

(define (build-choice rank if0 if1)
  (let (already memo-table) (dedup choice-memo rank if0 if1))
  (cond (already)
        (else
         (assert (< rank infinite-rank))
         (let index (ranks .push! rank))
         ( if0s .push if0)
         ( if1s .push if1)
         (memo-table .set! if1 index)
         index)))

(define (make-choice rank if0 if1)
  (if (= if0 if1)
      if0
      (build-choice rank if0 if1)))

(define (evaluate node env)             ;XXX name
  (cond ((<= node lit1) node)
        (else
         (let value (env (ranks node)))
         (evaluate ((ifs value) node) env))))

(define (do-choose node if0 if1)
  (cond ((<= node lit1)
         (case node
           (0 if0)                      ;N.B. 0 == lit0
           (1 if1)))
        ((= if0 if1)
         if0)
        ((and (= if0 lit0) (= if1 lit1))
         node)
        (else
         (choose node if0 if1))))

(define (subst rank replacement node)
  (case (rank .compare (ranks node))
    (-1 node)
    ( 0 (do-choose replacement (if0s node) (if1s node)))
    (+1 (make-choice (ranks node)
                     (subst rank replacement (if0s node))
                     (subst rank replacement (if1s node))))))

(let choose-memo (map<-))

(define (choose node if0 if1)
  (let (already memo-table) (dedup choose-memo node if0 if1))
  (cond (already)
        (else
         (assert (< lit1 node))
         (let top (min (ranks node) (ranks if0) (ranks if1)))
         (let on0 (do-choose (subst top lit0 node)
                             (subst top lit0 if0)
                             (subst top lit0 if1)))
         (let on1 (do-choose (subst top lit1 node)
                             (subst top lit1 if0)
                             (subst top lit1 if1)))
         (let result (make-choice top on0 on1))
         (memo-table .set! if1 result)
         result)))

(define (satisfy-first node goal)
  (let goal-node (constant<- goal))
  (let env (map<-))
  (recurse walking ((node node))
    (cond ((<= node lit1)
           (and (= node goal-node) env))
          (else
           (let if0 (if0s node))
           (cond ((or (< lit1 if0) (= if0 goal-node))
                  (env .set! (ranks node) 0)
                  (walking if0))
                 (else
                  (env .set! (ranks node) 1)
                  (walking (if1s node))))))))


;; dd.scm
;; Decision diagrams

(let infinity 999999)  ;; N.B. we don't have floats yet

(define (constant<- value)
  (make
    ({.rank}           infinity)
    ({.constant-value} value)
    ({.evaluate env}   value)
    ({.choose nodes}   (nodes value))))

(define (all-same? xs)
  (let set (set<-list xs))
  (= 1 set.count))

(define (choice<- rank branches)
  (assert (< rank infinity))
  (make choice
    ({.rank}           rank)
    ({.constant-value} #no)
    ({.evaluate env}   ((branches (env rank)) .evaluate env))
    ({.branches}       branches)
    ({.choose nodes}
     (cond ((all-same? nodes)
            (nodes 0))
           ((= (each '.constant-value nodes)
               (range<- nodes.count))
            choice)
           (else
            (memo-choice choice nodes))))))

(let memo-node (memoize choice<-))

(define (variable<- rank arity)
  (memo-node rank (each constant<- (range<- arity))))

(let memo-choice
  (memoize
   (given (node branches)
     (let top (minimum-by (cons node branches) '.rank))
     (let rank top.rank)
     (make-node rank
                (for each ((c (range<- top.branches.count)))
                  ((subst rank c node)
                   .choose (subst-each rank c branches)))))))

(define (make-node rank branches)
  (if (all-same? branches)
      (branches 0)
      (memo-node rank branches)))

(define (subst-each rank value nodes)
  (for each ((e nodes)) (subst rank value e)))

(define (subst rank value node)
  (cond ((< rank node.rank)
         node) ; N.B. node must be a constant, iff we arrived here (XXX why?)
        ((= rank node.rank)
         (node.branches value))
        (else
         (make-node node.rank
                    (subst-each rank value node.branches)))))

(define (valid? node)
  (not (satisfy node 0)))

;; XXX ugly
(define (satisfy node goal)
  (let env (map<-))
  (recurse walking ((node node))
    (if node.constant-value
        (and (= goal node.constant-value)
             env)
        (recurse trying ((rank node.rank)
                         (value 0)
                         (branches node.branches))
          (and (not branches.empty?)
               (cond ((`(#no ,goal) .maps-to? branches.first.constant-value)
                      (env .set! rank value)
                      (walking branches.first))
                     (else
                      (trying rank (+ value 1) branches.rest))))))))


;; fillvector.scm
;; growable mutable vectors (have a better name?)
;; TODO: shrink capacity sometimes

(let fillvector<-
  (given (& arguments)
    (fillvector<-vector (apply vector<- arguments))))

(define (fillvector<-count start-count start-value)
  (fillvector<-vector (vector<-count start-count start-value)))

(define (fillvector<-vector start-vector)   ;; (private)
  (let count (box<- start-vector.count))
  (let vec   (box<- start-vector))

  (define (grow)
    (let old (vec))
    (let n old.count)
    (vec .set! (vector<-count (if (= 0 n) 1 (* 2 n))))
    (vec .copy! old))

  (define (count-check i)
    (unless (< i (count))
      (error "Bad index" fillvector i)))

  (make fillvector
    ((i)
     (count-check i)
     ((vec) i))
    ({.count}
     (count))
    ({.set! i value}
     (count-check i)
     ((vec) .set! i value))
    ({.push! value}
     (let i (count))
     (if (= i ((vec) .count)) (grow) 'pass)
     ((vec) .set! i value)
     (count .set! (+ i 1))
     i)                              ; (should we be returning this?)
    ({.pop!}
     (let i (- (count) 1))
     (when (< i 0)
       (error "Underflow" fillvector))
     (count .set! i)
     ((vec) i))
    ({.snapshot}
     ((vec) .slice 0 (count)))         ;XXX make immutable
    ({.copy! v lo bound}
     (count-check bound)
     ((vec) .copy! v lo bound))

    ;; XXX should be vector trait...
    ({.empty?}         (= 0 (fillvector .count)))
    ({.first}          (fillvector 0))
    ({.rest}           (fillvector .slice 1))
    ({.copy! v}        (fillvector .copy! v 0 (v .count)))
    ({.slice lo}       (fillvector .slice lo (fillvector .count)))
    ;; inefficient:
    ({.chain v}        ((fillvector .snapshot) .chain v))
    ({.slice lo bound} ((fillvector .snapshot) .slice lo bound))
    ))


;; hashmap.scm
;; Hash-maps
;; Chances are we'll want these to be primitive, for use by vtables at least.

(load "stdlib.scm")

;; TODO:
;;   define equal?, 'hash -- mirandizing
;;   nonlinear probing
;;   preserving insertion order
;;   deletion
;;   define a special None value like Python?
;;   immutable snapshots
;;   hash-sets
;;
;;   impl without a million boxes
;;   N.B. impl needs shared closures for efficiency
;;        (capacity, occupants, ..., hashmap)
;;   special-case impls for small maps and common-typed maps
;;   store hash codes instead of recomputing?

(let empty (make))

(define (hashmap<-)
  (let count (box<- 0))
  (let keys  (box<- (vector<- empty)))  ;; size a power of 2
  (let vals  (box<- (vector<- #no)))     ;; same size

  (define (capacity) ((keys) .count))

  (define (occupants)
    (recurse walking ((i (- (capacity) 1)))
      (if (< i 0)
          '()
          (hide
           (let k ((keys) i))
           (if (= k empty)
               (walking (- i 1))
               (cons i (walking (- i 1))))))))

  (define (find key succeed fail)
    (let h    key.hash)              ;XXX coerce to fixnum
    (let mask (- ((keys) .count) 1))
    (let i0   (mask .and h))
    (recurse walking ((i i0))
      (let k ((keys) i))
      (cond ((= k empty)
             (fail i))
            ((= k key)             ;XXX
             (succeed i))
            (else
             (let j (mask .and (- i 1)))
             (if (= j i0)
                 (error "Can't happen")
                 (walking j))))))

  (define (maybe-grow)
    (when (< (* 2 (capacity))
             (* 3 (count)))
      (resize (* 2 (capacity)))))

  (define (resize new-capacity)
    (let old-keys (keys))
    (let old-vals (vals))
    (keys .set! (vector<-count new-capacity empty))
    (vals .set! (vector<-count new-capacity))
    (for each! ((i (range<- old-keys.count)))
      (let key (old-keys i))
      (unless (= key empty)
        (find key
              (given (j) (error "Can't happen"))
              (given (j)
                ((keys) .set! j key)
                ((vals) .set! j (old-vals i)))))))
                            
  (make hashmap
    ({.keys}   (each (keys) (occupants))) ;XXX lazy-map
    ({.values} (each (vals) (occupants)))
    ({.items}
     (let ks (keys))
     (let vs (vals))
     (for each ((i (occupants)))
       `(,(ks i) ,(vs i))))
    ({.empty?} (= (count) 0))
    ({.count}  (count))
    ((key)
     (find key (vals) (given (i) (error "Missing key" hashmap key))))
    ({.get key}
     (hashmap .get key #no))
    ({.get key default}
     (find key (vals) (given (i) default)))
    ({.set! key val}
     (find key
           (given (i)
             ((vals) .set! i val))
            (given (i)
              ((keys) .set! i key)
              ((vals) .set! i val)
              (maybe-grow))))
    ))


;; kanren.scm

(define (fail s)
  '())

(define (succeed s)
  `(,s))

(define ((== val1 val2) s)
  (let s1 (unify s val1 val2))
  (if s1 `(,s1) '()))

(define ((either goal1 goal2) s)
  (interleave (goal1 s) (goal2 s)))

;; TODO: probably ought to be lazy in the head as well as the tail
(define (interleave xs ys)
  (if xs.empty?
      ys
      (cons/lazy xs.first
                 (given () (interleave ys xs.rest)))))

(define ((both goal1 goal2) s)
  (each-chained/lazy goal2 (goal1 s)))

(define (cons/lazy x thunk)
  (make lazy-list
    ({.empty?} #no)
    ({.first} x)
    ({.rest} (thunk))  ;XXX memoize?
    ;; ... XXX use list-trait? except it'd need a rewrite for laziness
    ))

(define (each-chained/lazy f xs)
  (foldr/lazy (given (x rest-thunk) (chain/lazy (f x) rest-thunk))
              (given () '())
              xs))

(define (chain/lazy xs ys-thunk)
  (foldr/lazy cons/lazy ys-thunk xs))

(define (foldr/lazy f z-thunk xs)
  (if xs.empty?
      (z-thunk)
      (f xs.first
         (given () (foldr/lazy f xs.rest)))))


;; read.scm
;; A Lisp reader.
;; Planned extensions:
;; - Better error reporting
;; - Source-location tracking
;; - (Minor) new syntax like #yes/#no.

;; It's a drag to keep saying 'in-port' here, so let's just say 'port'.
;; TODO: better short names for in-port and out-port

;; XXX (read port default) is still not a POLA-respecting interface;
;; eof-object is better. But I'd prefer a uniform interface that's
;; like the answer for iterators, getting from a collection, etc.

(make eof)

(define (read-atom port char)
  XXX)

(let the-readtable (vector<-count 256 read-atom))

(define (set-read-macro char reader)
  (the-readtable .set! char.code reader))

(define (read port)
  (let char port.read-char))
  (if (= char eof)
      eof
      ((the-readtable char.code) port char)))

;; '(' tree* ')' -- where we've just eaten the '('.
;; XXX the extra must-read is ugly
(define (read-list port _)
  (recurse read-rest ()
    (skip-blanks port)
    (let peek port.peek-char)
    (cond ((= peek eof)
           (read-error port "Unexpected EOF in list"))
          ((= peek #\) ) 
           port.read-char
           '())
          (else 
           (cons (must-read port) (read-rest))))))

(define (skip-blanks port) ;; and comments too
  XXX)

(define (read-number port radix)
  XXX)

(define (read-error port plaint)
  (error (chain "Read error: " plaint)))

;; White space
(for each! ((white '(#\space #\tab #\newline #\return)))
  (set-read-macro white 
    (given (port _) 
      (skip-blanks port)
      (read port))))

(set-read-macro #\;
  (given (port _)
    (flush-input-line port)
    (read port)))

(set-read-macro #\( read-list)

(set-read-macro #\)
  (given (port _)
    (read-error port "Unbalanced parentheses")))

(let radix-map '((#\x 16) (#\X 16)      ;TODO: hashmap
                 (#\d 10) (#\D 10)
                 (#\o  8) (#\O  8)
                 (#\b  2) (@\B  2)))

(set-read-macro #\#
  (given (port _)
    (let peek port.peek-char)
    (cond ((assq peek radix-map) ;XXX can conflict with read-hash-symbol
           => (given (pair)
                port.read-char
                (read-number port (pair 1))))
          ((= peek #\\)
           port.read-char
           (read-char-literal port))
          (peek.alphabetic?
           (read-hash-symbol (read-symbol port)))
          ((= peek #\( )	; vector constant
           (vector<-list (read-list port port.read-char)))
          (else
           (read-error port "Unknown '#' read macro")))))

(set-read-macro #\"
  (given (port _)
    (recurse loop ((prev-chars '()))    ;TODO use growable-vector
      (let char port.read-char)
      (cond ((= char eof)
             (read-error port "Unexpected EOF in string constant"))
            ((= char #\")
             (string<-list (reverse prev-chars)))
            ((= char #\\)
             (let next port.read-char)
             (cond ((= next eof)
                    (read-error port "Unexpected EOF in escape sequence"))
                   ((assq next '((#\\ #\\)
                                 (#\" #\")
                                 (#\n #\newline)
                                 (#\t #\tab)
                                 (#\r #\return)))
                    => (given (pair)
		         (loop (cons (pair 1) prev-chars))))
                   (else
                    (read-error port 
                                "Unknown escape sequence in string"))))
            (else (loop (cons char prev-chars)))))))

(set-read-macro #\'
  (given (port _)
    `',(must-read port)))
    
(define (must-read port)
  (let tree (read port))
  (when (= tree eof)
    (read-error port "Unexpected EOF"))
  tree)

(set-read-macro #\`
  (given (port _)
    (list<- 'quasiquote (must-read port))))

(set-read-macro #\,
  (lambda (port char)
    (list<- (cond ((= port.peek-char #\@)
                   port.read-char
                   'unquote-splicing)
                  (else
                   'unquote))
            (must-read port))))


;; self-terp.scm

(define (constant<- value)
  (make
    ({.evaluate r} value)))

(define (variable<- name)
  (make
    ({.evaluate r} (r name))))

(define (make<- script)
  (make
    ({.evaluate r} (object<- script r)))) ;XXX needs expansion

(define (letrec<- defns body)
  (make
    ({.evaluate r}
     (let new-r (env-extend-promises r (map '.first defns)))
     (for each! ((defn defns))
       (env-resolve! new-r
                     (defn 0)
                     ((defn 1) .evaluate new-r)))
     (evaluate body new-r))))

(define (call<- cue addressee operands)
  (make
    ({.evaluate r}
     (call cue (addressee .evaluate r) (map '.evaluate operands))))))


;; spell1.scm
;; Norvig's (simpler) spelling corrector
;; http://norvig.com/spell-correct.html
;; TODO: try imitating https://en.wikibooks.org/wiki/Clojure_Programming/Examples/Norvig_Spelling_Corrector
;; TODO: uses pattern matching

(define (correct word)
  (let candidates (or (if-any (known `(,word)))
                      (if-any (known (edits1 word)))
                      (if-any (known-edits2 word))
                      `(,word)))
  (max-by candidates (given (w) (NWORDS .get w 0))))

(define (if-any xs)
  (if xs.empty? #no xs))

(define (known words)  ;TODO: iter instead of list? set comprehension?
  (set<-list (for filter ((w words))
               (NWORDS .maps? w))))

(define (known-edits2 word)
  (set<-list (for each-chained ((e1 (edits1 word)))
               (for filter ((e2 (edits1 e1)))
                 (NWORDS .maps? e2)))))

(define (edits1 word)      ;TODO: real list comprehensions should help
  (let splits     (for each ((i (range<- (+ word.count 1))))
                    `(,(word .slice 0 i)
                      ,(word .slice i))))
  (let deletes    (for each (((a b) (for filter (((a b) splits))
                                      (not b.empty?))))
                    (chain a (b .slice 1))))
  (let transposes (for each (((a b) (for filter (((a b) splits))
                                     (< 1 b.count))))
                    (chain a (string<- (b 1) (b 0)) (b .slice 2))))
  (let replaces   (for each-chained ((a b) splits)
                    (if b.empty?
                        '()
                        (for each ((c alphabet))
                          (chain a (string<- c) (b .slice 1))))))
  (let inserts    (for each-chained ((a b) splits)
                    (for each ((c alphabet))
                      (chain a (string<- c) b))))
  (set<-list (chain deletes transposes replaces inserts)))

(let alphabet "abcdefghijklmnopqrstuvwxyz")

(define (train features)
  (let model (map<-))               ;TODO
  (for each! ((f features))
    (model .set! f (+ 1 (model .get f 1))))
  model)

(define (words<-string string)
  (re:findall "[a-z]+" string.lowercase))  ;TODO

(let NWORDS (train (words<-string (call-with-open-file "big.txt" '.read-all))))


;; star2.scm
;; Like https://github.com/darius/regexercise_solutions/blob/master/star.py
;; ~/git/regexercise_solutions/star.py

(define (search re chars)
  (or (nullable? re)
      (hide
       (let states (box<- (set<- re)))
       (for some ((char chars))
         (states .set! (set<-sequence (for each-chained ((state (states)))
                                        (after state char))))
         (some nullable? (states))))))

(define (nullable? r)
  (case r
    ({empty}      #yes)
    ({literal _}  #no)
    ({either s t} (or (nullable? s) (nullable? t)))
    ({chain s t}  (and (nullable? s) (nullable? t)))
    ({star _}     #yes)))

;; Or equivalently:
(make nullable?
  (({empty})      #yes)
  (({literal _})  #no)
  (({either s t}) (or (nullable? s) (nullable? t)))
  (({chain s t})  (and (nullable? s) (nullable? t)))
  (({star _})     #yes))

(define (after r char)
  (case r
    ({empty}       '())
    ({literal lit} (if (= char lit) '({empty}) '()))
    ({either s t}  (chain (after s char) (after t char)))
    ({chain s t}
     (let dr+s (for each ((r-rest (after r char)))
                 (chain<- r-rest s)))
     (if (nullable? r)
         (chain dr+s (after s char))
         dr+s))
    ({star s}
     (for each ((r-rest (after r char)))
       (chain<- r-rest star)))))

(define (chain<- r s)
  (if (= r empty) s {chain r s}))


;; tictactoe.scm
;; tic-tac-toe, as a warmup.

(define (tic-tac-toe player opponent grid)
  (cond (grid.won?   (say grid.last-to-move " wins."))
        (grid.drawn? (say "A draw."))
        (else
         (unless (`(,player ,opponent) .maps-to? human-play)
           (display grid.show)
           (newline)
           (say player.show " to move " grid.whose-move
                ". (Press a key.)")
           (get-key))                    ;XXX
         (tic-tac-toe opponent player (player grid)))))

(define (human-play grid)
  "Just ask for a move."
  XXX)

(define (drunk-play grid)
  (minimum-by grid.successors drunk-value))

(define (spock-play grid)
  (minimum-by grid.successors spock-value))

(define (max-play grid)
  ; TODO: comparison of lists
  (minimum-by grid.successors
              (given (succ) `(,(spock-value succ) ,(drunk-value succ)))))

;; TODO: memoize
(define (drunk-value grid)
  (cond (grid.won? -1)
        (else
         (let succs grid.successors)
         (if succs.empty?
             0
             (- (average (each drunk-value succs)))))))
      
(define (spock-value grid)
  (cond (grid.won? -1)
        (else
         (let succs grid.successors)
         (if succs.empty?
             0
             (- (minimum (each spock-value succs)))))))

(define (average numbers)
  (/ (sum numbers) numbers.count))   ;TODO floats


(define (grid<- p q)

  (define (player-marks)
    (if (= (sum (player-bits p))
           (sum (player-bits q)))
        "XO"
        "OX"))

  (define (player-bits bits)
    (for each ((i (reverse (range<- 9)))) ;TODO: this is less efficient
      (1 .and (bits .>> i))))

  (make grid
    ({.won?}
     (for some ((way ways-to-win))
       (= way (way .and q))))
    ({.drawn?}
     grid.successors.empty?)
    ({.successors}
     (for filter-false ((move (range<- 9))) ;TODO better name
       (grid .move move)))
    ({.move move}
     (let bit (1 .<< move))
     (and (= 0 (bit .and (p .or q)))
          (grid<- q (p .or bit))))
    ({.whose-move}
     ((player-marks) 0))
    ({.last-to-move}
     ((player-marks) 1))
    ({.show}
     (let marks (player-marks))
     (call '.format grid-format
           (for each ((pair (zip (player-bits p) (player-bits q))))
             (case pair
               ((1 0) (marks 0))
               ((0 1) (marks 1))
               ((0 0) #\.)))))
    ))

(let grid-format ("\n" .join (for each ((_ (range<- 3)))
                               " %s %s %s")))

(let ways-to-win '(#o700 #o070 #o007 #o444 #o222 #o111 #o421 #o124))

(let empty-grid (grid<- 0 0))

(define (move<-human-numbered n)
  (- 9 n))

(define (sum ns)
  (foldl + 0 ns))

(define (filter-false xs)
  (filter identity xs))

(define (zip xs ys)
  (cond (xs.empty?
         (assert ys.empty? "Unequal list lengths" xs ys)
         '())
        (ys.empty?
         (error "Unequal list lengths" xs ys))
        (else
         (cons `(,xs.first ,ys.first)
               (zip xs.rest ys.rest)))))

(define (identity x)
  x)

(let say XXX)


;; um.scm
;; Universal Machine from http://www.boundvariable.org/task.shtml
;; Ported from https://github.com/darius/superbench
;; ~/git/superbench/um/um.lua
;; (using not-yet-implemented features and syntax)

;; TODO: s/vector/array?

;;XXX .u<op> means small, unsigned op
;; and I'm gonna assume 32-bit here
;; Similar for .s<op> (signed).
;; Do these mix OK? Is this reasonable?

;; TODO: mem could also be a typed vector, but neither the vector
;; nor the type is as simple...

(define (run program out-port in-port)
  (let mem (growable-vector<-))         ;TODO: shorter name? flexlist?
  (mem .push! program)
  (let free-list (growable-vector<-))
  (let reg (vector<-count 8 0))         ;TODO: typed vector, for efficiency

  (recurse running ((program program) (pc 0))

    (define (next)
      (running program (pc .u+ 1)))

    (let inst (program pc))
    (let opcode (inst .u>> 28))

    (case opcode
      (13 (reg .set! (7 .and (inst .u>> 25))
               (inst .and #x1FFFFFF))
          (next))
      (_
       (let a (7 .and (inst .u>> 6)))
       (let b (7 .and (inst .u>> 3)))
       (let c (7 .and inst))
       (case opcode

         (0 (when (not= (reg c) 0)
              (reg .set! a (reg b)))
            (next))

         (1 (reg .set! a ((mem (reg b))
                          (reg c)))
            (next))

         (2 ((mem (reg a)) .set! (reg b) (reg c))
            (next))

         (3 (reg .set! a ((reg b) .s+ (reg c)))
            (next))

         (4 (reg .set! a ((reg b) .s* (reg c)))
            (next))

         (5 (reg .set! a ((reg b) .u/ (reg c)))
            (next))

         (6 (reg .set! a (((reg b) .and (reg c)) .not))
            (next))

         (7 "Successful exit")

         (8 (let chunk (vector<-count (reg c) 0)) ;TODO: typed vector again
            (reg .set! b
                 (cond ((free-list .empty?)
                        (mem .push! chunk))
                       (else
                        (let i (free-list .pop!))
                        (mem .set! i chunk)
                        i)))
            (next))

         (9 (mem .set! (reg c) none)
            (free-list .push! (reg c))
            (next))

         (10 (out-port .write-char (char<- (reg c)))
             (next))

         (11 (let s in-port.read-char)
             (reg .set! c
                  (if (= s none) #xFFFFFFFF (string<- s)))
             (next))

         (12 (cond ((= (reg b) 0)
                    (running program (reg c)))
                   (else
                    (let new-program ((mem (reg b)) .shallow-copy))
                    (mem .set! 0 new-program)
                    (running new-program (reg c)))))

         (_ (out-port .writeln "Bad opcode")
            "Error exit"))))))

(define (read-program in-port)
  (let program (growable-vector<-))
  (recurse reading ()
    (let c3 in-port.read-char)
    (unless (= c3 none)
      (let c2 in-port.read-char)
      (let c1 in-port.read-char)
      (let c0 in-port.read-char)
      ;; TODO: a syntax for int-guard coercion instead?
      (program .push!
               (u32<-bytes c3.code c2.code c1.code c0.code))
      (reading)))))
  program)                              ;TODO: snapshot it

(define (u32<-bytes b3 b2 b1 b0)
  (append-byte (append-byte (append-byte b3 b2) b1) b0))

(define (append-byte u byte)
  (byte .u+ (u .u<< 8)))


;; unify.scm

(let variable? symbol?)                 ;XXX not really

(define (variable<- prefix n)
  (symbol<- ((chain prefix ".%d") .format n)))

(make empty-subst
  ({.subst val}
   val)
  ({.show}
   '()))

(define (extend-unchecked s my-var my-val)
  (make extended-subst
    ({.subst val}
     (if (variable? val)
         (if (= val my-var) my-val (s .subst val))
         val))
    ({.show}
     `((,my-var : ,my-val) ,@s.show))))

(define (extend s var val)
  (if (occurs? s var val) #no (extend-unchecked s var val)))

(define (occurs? s var val)
  (let val1 (s .subst val))
  (or (= var val1)
      (and (list? val1)
           (for some ((item val1))
             (occurs? s var item)))))

(define (unify s val1 val2)
  (let u (s .subst val1))
  (let v (s .subst val2))
  (cond ((= u v) s)
        ((variable? u)
         ((if (variable? v) extend-unchecked extend) s u v))
        ((variable? v)
         (extend s u v))
        ((and (list? u) (list? v) (= u.count v.count))
         (recurse unifying ((s s) (u u) (v v))
           (cond (u.empty? s)
                 (else
                  (let s1 (unify s u.first v.first))
                  (and s1 (unifying s1 u.rest v.rest))))))
        (else
         (and (= u v) s))))

(define (reify s val)
  (let free-vars (map<-))
  (recurse reifying ((val-in val))
    (let val (s .subst val))
    (cond ((variable? val)
           (unless (free-vars .maps? val)
             (free-vars .set! val
                        (variable<- "_" free-vars.count)))
           (free-vars val))
          ((list? val)
           (each reifying val))
          (else
           val))))

;; TODO: consider making a 'failed' subst type instead of #no
;; or using 0-or-1-length lists. In fact, the latter meshes
;; perfectly with lazy-lists-as-Kanren-results.
