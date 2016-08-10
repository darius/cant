;;; Trying out the new syntax scheme, with a little new semantics.

;;; pair.scm

(let (pair? pair-stamp) (stamp<- "pair"))

(define (cons head tail)
  ;; XXX where do we define the selflessness and equality?
  ;;     also, the pattern-matching syntax?
  (make pair
    {stamped pair-stamp}
    {extending list-trait}              ;XXX syntax?
    ({.empty?} #no)
    ({.first}  head)
    ({.rest}   tail)))


;;; stdlib.scm

(make +
  (() 0)
  ((a) a)
  ((a b) (a .+ b))
  ((@arguments) (foldr1 '.+ arguments)))

(make *
  (() 1)
  ((a) a)
  ((a b) (a .* b))
  ((@arguments) (foldr1 '.* arguments)))

(make -
  (() (error "Bad arity"))
  ((a) (0 .- a))
  ((a b) (a .- b))
  ((@arguments) (foldl '.- arguments.first arguments.rest)))

(define (vector<-list xs)
  (let v (vector<-count xs.count))
  (for each! ((i x) (enumerate xs))
    (v .set! i x))
  v)

;; XXX float contagion
(define (min x y) (if (< x y) x y))
(define (max x y) (if (< x y) y x))


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
  (match lexp
    ((: symbol?)
     (var-ref<- lexp))
    ((: number?)
     (constant<- lexp))
    (('lambda (v) body)
     (abstraction<- v (parse body)))
    ((operator operand)
     (call<- (parse operator)
             (parse operand)))))

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
                ({.survey} `(,v -> <body>))
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
  (case ((assq v r) => (given (record) (k .take (record 1))))
        (else (debug k "Unbound var" v))))


;; Debugger
;; Instead of interacting at a prompt, it takes a list of commands,
;; for now, for ease of rerunning during development.

(let command-queue (box<- '()))

(define (next-command)
  (display "debug> ")
  (match command-queue.^
    (()
     (newline)
     #no)
    ((first @rest)
     (print first)
     (command-queue .^= rest)
     first)))

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
        ({.take value} (debugging (out-step<- k value)))
        ;; XXX (.inject ...) ?
        (else message (call k message)))))


;; Smoke test

(make try
  ((lexp)
   (try lexp '()))
  ((lexp commands)
   (command-queue .^= commands)
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


;; bdd.scm
;; from ~/git/mccarthy-to-bryant/lua/bdd3.lua

(let lit0 0)
(let lit1 1)

(define (constant<- value)
  (assert (or (= value lit0) (= value lit1))
          "Not binary" value)
  value)

(let infinite-rank 0x7fffffff)
(let ranks (fillvector<- infinite-rank infinite-rank))
(let if0s  (fillvector<- lit0 lit1))
(let if1s  (fillvector<- lit0 lit1))
(let ifs   (vector<- if0s if1s))

(define (dedup memo k1 k2 k3)
  (define (enter map key)
    (or (map .get key)
        (do (let v (map<-))
            (map .set! key v)
            v)))
  (let mem1 (enter memo k1))
  (let mem2 (enter mem1 k2))
  (list<- (mem2 .get k3) mem2))

(let choice-memo (map<-))

(define (build-choice rank if0 if1)
  (let (already memo-table) (dedup choice-memo rank if0 if1))
  (or already
      (do (assert (< rank infinite-rank))
          (let index (ranks .push! rank))
          (if0s .push! if0)
          (if1s .push! if1)
          (memo-table .set! if1 index)
          index)))

(define (make-choice rank if0 if1)
  (if (= if0 if1)
      if0
      (build-choice rank if0 if1)))

(define (evaluate node env)             ;XXX name
  (case ((<= node lit1) node)
        (else
         (let value (env (ranks node)))
         (evaluate ((ifs value) node) env))))

(define (do-choose node if0 if1)
  (case ((<= node lit1)
         (match node
           (0 if0)                      ;N.B. 0 == lit0
           (1 if1)))
        ((= if0 if1)
         if0)
        ((and (= if0 lit0) (= if1 lit1))
         node)
        (else
         (choose node if0 if1))))

(define (subst rank replacement node)
  (match (rank .compare (ranks node))
    (-1 node)
    ( 0 (do-choose replacement (if0s node) (if1s node)))
    (+1 (make-choice (ranks node)
                     (subst rank replacement (if0s node))
                     (subst rank replacement (if1s node))))))

(let choose-memo (map<-))

(define (choose node if0 if1)
  (let (already memo-table) (dedup choose-memo node if0 if1))
  (or already
      (do (assert (< lit1 node))
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
  (begin walking ((node node))
    (case ((<= node lit1)
           (and (= node goal-node) env))
          (else
           (let if0 (if0s node))
           (case ((or (< lit1 if0) (= if0 goal-node))
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
     (case ((all-same? nodes)
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
  (case ((< rank node.rank)
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
  (begin walking ((node node))
    (if node.constant-value
        (and (= goal node.constant-value)
             env)
        (begin trying ((rank node.rank)
                       (value 0)
                       (branches node.branches))
          (and (not branches.empty?)
               (case ((`(#no ,goal) .maps-to? branches.first.constant-value)
                      (env .set! rank value)
                      (walking branches.first))
                     (else
                      (trying rank (+ value 1) branches.rest))))))))


;; fillvector.scm
;; growable mutable vectors (have a better name?)
;; TODO: shrink capacity sometimes

(define (fillvector<- @arguments)
  (fillvector<-vector (apply vector<- arguments)))

(define (fillvector<-count start-count start-value)
  (fillvector<-vector (vector<-count start-count start-value)))

(define (fillvector<-vector start-vector)   ;; (private)
  (let count (box<- start-vector.count))
  (let vec   (box<- start-vector))

  (define (grow)
    (let old vec.^)
    (let n old.count)
    (vec .^= (vector<-count (if (= 0 n) 1 (* 2 n))))
    (vec.^ .copy! old))

  (define (count-check i)
    (unless (< i count.^)
      (error "Bad index" fillvector i)))

  (make fillvector
    ((i)
     (count-check i)
     (vec.^ i))
    ({.count}
     count.^)
    ({.set! i value}
     (count-check i)
     (vec.^ .set! i value))
    ({.push! value}
     (let i count.^)
     (if (= i vec.^.count) (grow) 'pass)
     (vec.^ .set! i value)
     (count .^= (+ i 1))
     i)                              ; (should we be returning this?)
    ({.pop!}
     (let i (- count.^ 1))
     (when (< i 0)
       (error "Underflow" fillvector))
     (count .^= i)
     (vec.^ i))
    ({.snapshot}
     (vec.^ .slice 0 count.^))         ;XXX make immutable
    ({.copy! v lo bound}
     (count-check bound)
     (vec.^ .copy! v lo bound))

    ;; XXX should be vector trait...
    ({.empty?}         (= 0 fillvector.count))
    ({.first}          (fillvector 0))
    ({.rest}           (fillvector .slice 1))
    ({.copy! v}        (fillvector .copy! v 0 v.count))
    ({.slice lo}       (fillvector .slice lo fillvector.count))
    ;; inefficient:
    ({.chain v}        (fillvector.snapshot .chain v))
    ({.slice lo bound} (fillvector.snapshot .slice lo bound))
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
  (gather/lazy goal2 (goal1 s)))

(define (cons/lazy x thunk)
  (make lazy-list
    ({.empty?} #no)
    ({.first} x)
    ({.rest} (thunk))  ;XXX memoize?
    ;; ... XXX use list-trait? except it'd need a rewrite for laziness
    ))

(define (gather/lazy f xs)
  (for foldr/lazy ((x xs)
                   (rest-thunk (given () '())))
    (chain/lazy (f x) rest-thunk)))

(define (chain/lazy xs ys-thunk)
  (foldr/lazy cons/lazy xs ys-thunk))

(define (foldr/lazy f xs z-thunk)
  (if xs.empty?
      (z-thunk)
      (f xs.first
         (given () (foldr/lazy f xs.rest z-thunk)))))


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
  (let char port.read-char)
  (if (= char eof)
      eof
      ((the-readtable char.code) port char)))

;; '(' tree* ')' -- where we've just eaten the '('.
;; XXX the extra must-read is ugly
(define (read-list port _)
  (begin reading ()
    (skip-blanks port)
    (let peek port.peek-char)
    (case ((= peek eof)
           (read-error port "Unexpected EOF in list"))
          ((= peek #\) ) 
           port.read-char
           '())
          (else 
           (cons (must-read port) (reading))))))

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
                 (#\b  2) (#\B  2)))

(set-read-macro #\#
  (given (port _)
    (let peek port.peek-char)
    (case ((assq peek radix-map) ;XXX can conflict with read-hash-symbol
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
    (begin scanning ((prev-chars '()))    ;TODO use growable-vector
      (let char port.read-char)
      (case ((= char eof)
             (read-error port "Unexpected EOF in string constant"))
            ((= char #\")
             (string<-list (reverse prev-chars)))
            ((= char #\\)
             (let next port.read-char)
             (case ((= next eof)
                    (read-error port "Unexpected EOF in escape sequence"))
                   ((assq next '((#\\ #\\)
                                 (#\" #\")
                                 (#\n #\newline)
                                 (#\t #\tab)
                                 (#\r #\return)))
                    => (given (pair)
		         (scanning (cons (pair 1) prev-chars))))
                   (else
                    (read-error port 
                                "Unknown escape sequence in string"))))
            (else (scanning (cons char prev-chars)))))))

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
  (given (port _)
    (list<- (case ((= port.peek-char #\@)
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

(define (call<- addressee cue operands) ;XXX not necessarily with a cue anymore
  (make
    ({.evaluate r}
     (call (addressee .evaluate r) cue (map '.evaluate operands)))))


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
  (set<-list (for gather ((e1 (edits1 word)))
               (for filter ((e2 (edits1 e1)))
                 (NWORDS .maps? e2)))))

(define (edits1 word)
  (let splits     (for each ((i (range<- (+ word.count 1))))
                    `(,(word .slice 0 i)
                      ,(word .slice i))))
  (let deletes    (for each (((a b) (for filter (((a b) splits))
                                      (not b.empty?))))
                    (chain a (b .slice 1))))
  (let transposes (for each (((a b) (for filter (((a b) splits))
                                      (< 1 b.count))))
                    (chain a (string<- (b 1) (b 0)) (b .slice 2))))
  (let replaces   (for gather ((a b) splits)
                    (if b.empty?
                        '()
                        (for each ((c alphabet))
                          (chain a (string<- c) (b .slice 1))))))
  (let inserts    (for gather ((a b) splits)
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
      (do (let states (box<- (set<- re)))
          (for some ((char chars))
            (states .^= (set<-sequence (for gather ((state states.^))
                                         (after state char))))
            (some nullable? states.^)))))

(define (nullable? r)
  (match r
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
  (match r
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
  (if (= r {empty}) s {chain r s}))


;; tictactoe.scm
;; tic-tac-toe, as a warmup.

(define (tic-tac-toe player opponent grid)
  (case (grid.won?   (say grid.last-to-move " wins."))
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
  (if grid.won?
      -1
      (match grid.successors
        (() 0)
        (succs (- (average (each drunk-value succs)))))))

(define (spock-value grid)
  (if grid.won?
      -1
      (match grid.successors
        (() 0)
        (succs (- (minimum (each spock-value succs)))))))

(define (average numbers)
  (/ (sum numbers) numbers.count))   ;TODO floats


(define (grid<- p q)

  (define (player-marks)
    (if (= (sum (player-bits p))
           (sum (player-bits q)))
        "XO"
        "OX"))

  (define (player-bits bits)
    (for each ((i (range<- 9)))
      (1 .and (bits .>> i))))

  (make grid
    ({.won?}
     (for some ((way ways-to-win))
       (= way (way .and q))))
    ({.drawn?}
     grid.successors.empty?)
    ({.successors}
     (filter-false                      ;TODO better name
      (for each ((move (range<- 9)))
        (grid .move move))))
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
     (call (method<- grid-format '.format) ;XXX make format a fn instead
           (for each ((pair (reverse (zip (player-bits p)
                                          (player-bits q)))))
             (match pair
               ((1 0) (marks 0))
               ((0 1) (marks 1))
               ((0 0) #\.)))))
    ))

(define (method<- actor cue)
  (given (@arguments)
    (call actor (term<- cue arguments))))

(let grid-format ("\n" .join (for each ((_ (range<- 3)))
                               " %s %s %s")))

(let ways-to-win '(0o700 0o070 0o007 0o444 0o222 0o111 0o421 0o124))

(let empty-grid (grid<- 0 0))

(define (move<-human-numbered n)
  (- 9 n))

(define (sum ns)
  (foldl + 0 ns))

(define (filter-false xs)
  (filter identity xs))

(define (zip xs ys)
  (match `(,xs ,ys)
    ((() ()) '())
    (((x @xs1) (y @ys1))
     `((,x ,y) ,@(zip xs1 ys1)))))

(define (identity x)
  x)

(define (say @arguments) ;XXX I want @arguments to match only a list of arguments
  (each! display arguments)
  (newline))


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
  (let mem (fillvector<-))         ;TODO: shorter name? flexlist?
  (mem .push! program)
  (let free-list (fillvector<-))
  (let reg (vector<-count 8 0))         ;TODO: typed vector, for efficiency

  (begin running ((program program) (pc 0))

    (define (next)
      (running program (pc .u+ 1)))

    (let inst (program pc))
    (let opcode (inst .u>> 28))

    (match opcode
      (13 (reg .set! (7 .and (inst .u>> 25))
               (inst .and 0x1FFFFFF))
          (next))
      (_
       (let a (7 .and (inst .u>> 6)))
       (let b (7 .and (inst .u>> 3)))
       (let c (7 .and inst))
       (match opcode

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
                 (case (free-list.empty?
                        (mem .push! chunk))
                       (else
                        (let i free-list.pop!)
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
                  (if (= s none) 0xFFFFFFFF (string<- s)))
             (next))

         (12 (case ((= (reg b) 0)
                    (running program (reg c)))
                   (else
                    (let new-program ((mem (reg b)) .shallow-copy))
                    (mem .set! 0 new-program)
                    (running new-program (reg c)))))

         (_ (out-port .writeln "Bad opcode")
            "Error exit"))))))

(define (read-program in-port)
  (let program (fillvector<-))
  (begin reading ()
    (let c3 in-port.read-char)
    (unless (= c3 none)
      (let c2 in-port.read-char)
      (let c1 in-port.read-char)
      (let c0 in-port.read-char)
      ;; TODO: a syntax for int-guard coercion instead?
      (program .push!
               (u32<-bytes c3.code c2.code c1.code c0.code))
      (reading)))
  program)                              ;TODO: snapshot it

(define (u32<-bytes b3 b2 b1 b0)
  (append-byte (append-byte (append-byte b3 b2) b1) b0))

(define (append-byte u byte)
  (byte .u+ (u .u<< 8)))


;; unify.scm

(let variable? symbol?)                 ;XXX not really

(define (variable<- prefix n)
  (symbol<- (chain prefix (format ".%d" n)))) ;XXX define format

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
  (case ((variable? u)
         ((if (variable? v) extend-unchecked extend) s u v))
        ((variable? v)
         (extend s u v))
        ((and (list? u) (list? v) (= u.count v.count))
         (begin unifying ((s s) (u u) (v v))
           (if u.empty?
               s
               (do (let s1 (unify s u.first v.first))
                   (and s1 (unifying s1 u.rest v.rest))))))
        (else
         (and (= u v) s))))

(define (reify s val)
  (let free-vars (map<-))
  (begin reifying ((val-in val))
    (let val (s .subst val))
    (case ((variable? val)
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
