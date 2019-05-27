;; Trivial Squirm interpreter, trampolined style

;; TODO:
;; * how to represent processes with mailboxes? that part won't naturally
;;   be purely functional
;;   * I guess it is natural to stick the current process structure
;;     (mutable) in the current k. It could be a pair of ID and inbox.
;;     A language-level PID datum could be the same object.

(import (use "lib/queue")
  empty push peek)

(to (run module @(optional entry arguments))
  (squirm-run module (or entry 'main) (or arguments '())))

(to (squirm-run module entry arguments)
  (let state {go {start entry (module-env<- (module-parse module))}
                 arguments})
  (running (enqueue empty state)))

(to (running q)
  (match (peek q)
    ({empty} 'done)
    ({nonempty state q2}
     (match state
       ({go k value}
        (running (enqueue q2 (go k value))))))))

(to (enqueue q state)
  (match state
    ({go _ _}
     (push q state))
    ({fork state1 state2}
     (enqueue (enqueue q state1) state2))
    ({halt}
     q)))

(to (module-parse module)
  (each def-parse module))

(to (def-parse def)
  (match def
    (`(to (,name ,@params) ,@body)
     {to name params (seq-parse body)})))

(to (seq-parse exps)
  (match exps
    (`(,e)
     (exp-parse e))
    (`((let ,p ,e) ,@es)
     (exp-parse `((given (,p) ,@es) ,e)))
    (`(,e ,@es)
     {then (exp-parse e) (seq-parse es)})))

(to (exp-parse e)
  (match e
    ((? symbol?)          {var e})
    ((? self-evaluating?) {const e})
    (`',value             {const value})
    (`(given ,ps ,@body)
     {given ps (seq-parse body)}) ;TODO patterns
    (`(if ,t ,y ,n)
     {if (exp-parse t) (exp-parse y) (exp-parse n)})
    (`(do ,@es)
     (seq-parse es))
    (`(? ,@clauses)
     {receive (each clause-parse clauses)})
    (`(,operator ,@operands)
     {call (exp-parse operator) (each exp-parse operands)})))

(to (clause-parse clause)
  (let `(,pattern ,@seq) clause)
  {clause (pattern-parse pattern) (seq-parse seq)})

(to (pattern-parse pattern)
  (match pattern
    ((? symbol?) pattern)))             ;TODO: more

;; TODO: macros

;; sev: squirm evaluate
;; exp: expression
;; r: environment
;; k: continuation
(to (sev exp r k)
;;  (print `(sev ,exp))
  (match exp
    ({const value}
     {go k value})
    ({var name}
     {go k (env-get r name)})           ;TODO error handling
    ({given ps e}
     {go k {closure r ps e}})
    ({call e es}
     (sev e r {ev-operands es r k}))
    ({if e _ _}                  ;TODO base on match instead
     (sev e r {branch exp r k}))
    ({then e1 e2}
     (sev e1 r {then-drop e2 r k}))
    ({receive clauses}
     (error "TODO")) 
   ;; TODO: match
    ))

(to (go k value)
;;  (print `(go ,k ,value))
  (match k
    ({ev-operands es r k}
     (ev-operands value '() es r k))
    ({ev-more-operands f rev-args es r k}
     (ev-operands f (cons value rev-args) es r k))
    ({branch {if _ y n} r k}
     (sev (if value y n) r k))
    ({then-drop e2 r k}
     (sev e2 r k))
    ({start entry r}
     (sev {call {var entry} (for each ((arg value)) {const arg})}
          r
          {halt}))
    ({halt}
     {halt})
    ))

(to (ev-operands f rev-args operands r k)
  (match operands
    ('()
     (apply f (reverse rev-args) k))
    (`(,e ,@es)
     (sev e r {ev-more-operands f rev-args es r k}))))

(to (apply f args k)
  (match f
    ({closure r ps e}
     (surely (= args.count ps.count))
     (sev e (env-extend r ps args) k))
    ({primitive p}
     {go k (call p args)})
    ({spawn}
     (let `(,f1) args)
     (let new-pid 'XXX)                 ;TODO
     {fork {go k new-pid}
           (apply f1 '() {halt})})
    ))

(to (env-extend r ps vals)
  (surely (every symbol? ps))      ;TODO patterns
  {local-env (map<- (zip ps vals))
             r})

(to (env-get r name)
  (match r
    ({module-env map}
     (match (map .get name)
       (#no (global-map name))          ;TODO error handling
       ({to f params body}
        {closure r params body})))
    ({local-env map parent}
     (match (map .get name)
       (#no (env-get parent name))
       (value value)))))

;; TODO renamings: s/array/tuple
;; TODO special prims: ! eval apply error me throw catch ...
;; TODO file I/O, networks, time
;; TODO squeam methods as prims

(let primitives-from-squeam
  '(print display newline read
    cons chain 
    null? cons? list? number? integer? symbol? claim? char? string? array?
    symbol<- char<-
    number<-string string<-number list<-string self-evaluating? 
    inexact<-exact exact<-inexact floor not assoc sqrt
    < = > <= >= not= 
    * / + - expt abs gcd
    ))

(let global-map
  (map<- (for each ((name primitives-from-squeam))
           `(,name {primitive ,(evaluate name '())}))))

(global-map .set! 'spawn {spawn})

(to (module-env<- module)
  {module-env (map<- (for each ((def module))
                       (match def
                         ({to name _ _} `(,name ,def)))))})

(to (smoke-test)
  (run '((to (main)
           (spawn (given () (print "hey") (print (f 15))))
           (print (f 10)))
         (to (f n)
           (if (= n 0)
               1
               (do (let x (f (- n 1)))
                   (* n x)))))))
(smoke-test)
