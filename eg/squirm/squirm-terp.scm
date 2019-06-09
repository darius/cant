;; Basic Squirm interpreter as a testable sketch of the semantics

(import (use "lib/queue")
  empty empty? push peek extend list<-queue)


;; Main

(to (squirm-main argv)
  (let `(,_ ,filename) argv)
  (run-file filename))

(to (run-file filename @(optional entry arguments))
  (let module (with-input-file read-all filename))
  (run module entry arguments))

(to (run module @(optional entry arguments))
  (squirm-run module (or entry 'main) (or arguments '())))

(to (squirm-run module entry arguments)
  (run-queue .^= empty)
  (spawn (env-get (module-env<- (module-parse module)) entry)
         arguments)
  (running))


;; Processes, scheduling, message passing

(to (spawn f @(optional arguments))
  (let new-process (process<- {go {spawn f}
                                  (or arguments '())}))
  (run-queue .^= (push run-queue.^ new-process))
  new-process)

(to (running)
  (match (peek run-queue.^)
    ({empty} 'done)
    ({nonempty pid q2}
     (run-queue .^= q2)
     pid.run-a-slice
     (running))))

(let run-queue (box<- empty))
(let the-running-process (box<- #no))

(let pid-counter (box<- 0))

(to (process<- start-state)
  (let pid-num pid-counter.^)           ; Just for display.
  (pid-counter .^= (+ pid-num 1))
  (let state (box<- start-state))       ; State of execution.
  (let inbox-checked (box<- empty))     ; Messages that didn't match the current receive.
  (let inbox-unchecked (box<- empty))   ; Messages not yet checked against the current receive.
  (let watchers (set<-))                ; Monitors to notify on my exit.

  (make process

    ({.selfie sink}
     (format .to-sink sink "#<~w>" pid-num))

    ({.subscribe watcher}
     (watchers .add! watcher))

    ({.unsubscribe watcher}
     (watchers .delete! watcher))

    ({.receive-signal pid outcome}
     ;; TODO: links vs. monitors
     (process .enqueue (array<- 'DOWN pid outcome)))

    ({.enqueue message}
     (inbox-unchecked .^= (push inbox-unchecked.^ message))
     (match state.^
       ((and {blocked _ _ _} thunk)
        (run-queue .^= (push run-queue.^ process))
        (state .^= {go thunk #no})) ;(TODO still a bit clumsy)
       (_)))

    ({.receive (and exp {receive clauses}) r k}
     ;; TODO: error if exited? Probably not.
     (begin checking ()  ;; TODO finer time-slicing?
       (match (peek inbox-unchecked.^)
         ({empty}
          {blocked exp r k})
         ({nonempty msg rest}
          (inbox-unchecked .^= rest)
          (let map (map<-))
          (match (match-clauses r map clauses msg)
            (#no
             (inbox-checked .^= (push inbox-checked.^ msg))
             (checking))
            ({clause _ exp}
             (inbox-unchecked .^= (extend inbox-checked.^ (list<-queue rest)))
             (inbox-checked .^= empty)
             (sev exp {local-env map r} k)))))))

    ({.run-a-slice}
     (match state.^
       ({go k value}
        (the-running-process .^= process)
        (let state2 (go k value))
        (the-running-process .^= #no)
        (state .^= state2)
        (match state2
          ({blocked a b c}
           (surely (empty? inbox-unchecked.^) "inbox populated"))
          ({exit outcome}
           (for each! ((watcher watchers.keys))
             (watcher .receive-signal process outcome))) ;TODO pass more info
          (_ (run-queue .^= (push run-queue.^ process)))))
       ({exit _}
        (surely #no))
       ({blocked _ _ _}
        (surely (empty? inbox-unchecked.^) "I'm supposed to be blocked"))
       ))))


;; Modules

(let modules (map<-))

(to (module-ref mod-name var-name)
  (env-get (modules mod-name) var-name)) ;TODO lazy loading, reloading, etc.

(to (module-load mod-name filename)
  (surely (not (modules .maps? mod-name))) ;XXX for now
  (let defs (with-input-file read-all filename))  
  (modules .set! mod-name (module-env<- (module-parse defs))))
  

;; Language syntax

(to (module-parse module)
  (each def-parse module))

(to (def-parse def)
;;  (print `(def-parse ,def))
  (match def
    (`(to (,(? symbol? name) ,@params) ,@body)
     {to name (each pat-parse params) (seq-parse body)})
    (`(to (,(? cons? nested) ,@params) ,@body)
     (def-parse `(to ,nested (given ,params ,@body))))
    ))

(to (seq-parse exps)
;;  (print `(seq-parse ,exps))
  (match exps
;; Not sure we want this:
;;    ('()
;;     (exp-parse #no))
    (`(,e)
     (exp-parse e))
    (`((let ,p ,e) ,@es)
     (exp-parse `((given (,p) ,@es) ,e)))
    (`((define ,@defs) ,@es)
     {define (each def-parse defs) (seq-parse es)})
    (`((to ,@_) ,@es)
     (seq-parse `((define ,exps.first) ,@es)))
    (`(,e ,@es)
     {then (exp-parse e) (seq-parse es)}))) ;TODO parse as ((let _ ,e) .@es) ?

(to (exp-parse e)
  (match e
    ((? symbol?)
     (if (module-ref? e)
         (module-ref-parse e)
         {var e}))
    ((? self-evaluating?) {const e})
    (`',value             {const value})
    ((? array?)
     {call {const prim-tuple<-} (each exp-parse e)})
    (`(given ,ps ,@body)
     {given (each pat-parse ps) (seq-parse body)})
    (`(if ,t ,y ,n)
     ;; TODO macroexpand to a (match ...) instead?
     {if (exp-parse t) (exp-parse y) (exp-parse n)})
    (`(do ,@es)
     (seq-parse es))
    (`(? ,@clauses)
     {receive (each clause-parse clauses)})
    (`(match ,subject ,@clauses)
     {match (exp-parse subject) (each clause-parse clauses)})
    (`(catch ,@es)   ;; TODO macroexpand into (%catch (given () e)) ?
     {catch (seq-parse es)})
    (`(,operator ,@operands)
     (match (exp-macro-expand operator operands)
       (#no {call (exp-parse operator) (each exp-parse operands)})
       (expanded (exp-parse expanded))))))

(to (exp-macro-expand operator operands) ;TODO: make it extensible
  ;; TODO: quasiquote
  (match operator
    ('and
     (match operands
       ('() #no)
       (`(,e) e)
       (`(,e ,@es) `(if ,e (and ,@es) #no))))
    ('begin
        ;; (begin f ((x a) (y b)) e) => (hide (to (f a b) e) (f x y))
     (match operands
       (`(,f ,pairs ,@body)
        (for each! ((pair pairs))
          (surely (and (list? pair) (= pair.count 2))))
        `(do
           ;; TODO local to-defs in seqs
           (to (,f ,@(each '.first pairs))
             ,@body)
           (f ,@(for each ((pair pairs)) (pair 1)))))))
    ('case
     (match operands
       ('()
        '(exit "No true case")) ;TODO hygiene, & settle on what to exit with
       (`((else ,@seq))
        `(do ,@seq))
       (`((,test ,@seq) ,@clauses)
        `(if ,test
             (do ,@seq)
             (case ,@clauses)))))
    ('for
        ;;(for f ((x a) (y b)) e) => (f (given (a b) e) x y)
     (match operands
       (`(,f ,pairs ,@body)
        (for each! ((pair pairs))
          (surely (and (list? pair) (= pair.count 2))))
        `(,f (given ,(each '.first pairs) ,@body)
             ,@(for each ((pair pairs)) (pair 1))))))
    ('or
     (match operands
       ('() #yes)
       (`(,e) e)
       (`(,e ,@es)
        `(match ,e
           (#no (or ,@es))
           (yeah yeah)))))
    ('unless
     (match operands
       (`(,test ,@body)
        `(match ,test
           (#no ,@body)
           (_ #no)))))
    ('when
     (match operands
       (`(,test ,@body)
        `(match ,test
           (#no #no)
           (_ ,@body)))))
    (_ #no)))

;; Transform foo:bar to {module-ref foo bar}
;; (TODO better to do this in the reader in the real system.)
(to (module-ref? symbol)
  (symbol.name .find? #\:))

(to (module-ref-parse symbol)
  (let `(,mod ,var) (symbol.name .split ":"))
  {module-ref (symbol<- mod) (symbol<- var)})

(to (clause-parse clause)
  (let `(,pattern ,@seq) clause)
  {clause (pat-parse pattern) (seq-parse seq)})

(to (pat-parse pattern)
  (match pattern
    ((? symbol?)
     (surely (not (module-ref? pattern)))
     (if (pattern.name .starts-with? "_")
         {ignore}
         {bind pattern}))
    ((? self-evaluating?)
     {expect pattern})
    ((? array?)
     {tuple-pat (each pat-parse pattern)})
    (`',value
     {expect value})
    (`(link ,pf ,pr)
     {link (pat-parse pf) (pat-parse pr)})
    (`(list ,@ps)
     (pat-parse (if ps.empty?
                    ''()
                    `(link ,ps.first (list ,@ps.rest)))))
    (`(: ,(? symbol? name))
     {expect-var name})
    ))


;; Interpreter in trampolined style

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
    ({module-ref mod-name var-name}
     {go k (module-ref mod-name var-name)}) ;XXX module-ref might error or block
    ({given ps e}
     {go k {closure r ps e}})
    ({call e es}
     (sev e r {ev-operands es r k}))
    ({if e _ _}                  ;TODO base on match instead
     (sev e r {branch exp r k}))
    ({then e1 e2}
     (sev e1 r {then-drop e2 r k}))
    ({receive _}
     (the-running-process.^ .receive exp r k))
    ({match e clauses}
     (sev e r {matching clauses r k}))
    ({catch e}
     (sev e r {catch-frame k}))
    ({define defs e}
     (sev e {recursive-env (map<-defs defs) r} k))
    ))

(to (go kk value)
;;  (print `(go ,kk ,value))
  (match kk
    ({ev-operands es r k}
     (ev-operands value '() es r k))
    ({ev-more-operands f rev-args es r k}
     (ev-operands f (cons value rev-args) es r k))
    ({branch {if _ y n} r k}
     (sev (if value y n) r k))
    ({then-drop e2 r k}
     (sev e2 r k))
    ({spawn f}
     (apply f value {halt}))
    ({blocked exp r k}
     (sev exp r k))
    ({matching clauses r k}
     (let map (map<-))                  ;TODO factor dupe?
     (match (match-clauses r map clauses value)
       (#no (exit "Match failure"))
       ({clause _ e}
        (sev e {local-env map r} k))))
    ({catch-frame k}
     {go k value})              ;TODO distinguish from thrown outcome?
    ({halt}
     {exit 'normal})                    ;TODO design for exit data
    ))

(to (throw kk outcome)
  (match kk                             ;TODO generic walk through k's
    ({ev-operands es r k}
     (throw k outcome))
    ({ev-more-operands f rev-args es r k}
     (throw k outcome))
    ({branch _ _ k}
     (throw k outcome))
    ({then-drop e2 r k}
     (throw k outcome))
    ({matching clauses r k}
     (throw k outcome))
    ({catch-frame k}
     {go k outcome})                    ;TODO distinguish from non-exception result?
    ({halt}
     {exit outcome})))

(to (exit k reason)
  (throw k (array<- 'exit reason)))

(to (ev-operands f rev-args operands r k)
  (match operands
    ('()
     (apply f (reverse rev-args) k))
    (`(,e ,@es)
     (sev e r {ev-more-operands f rev-args es r k}))))

(to (apply f args k)
  (match f
    ({closure r ps e}
     (surely (= args.count ps.count) "arity mismatch")
     (let map (map<-))
     (match (match-pats r map ps args)
       (#no
        (exit k "Match failure"))
       (#yes
        (sev e {local-env map r} k))))
    ({primitive p}
     (apply-primitive p args k))
    ({apply}
     (match args
       (`(,f1 ,args1)
        (apply f1 args1 k))))
    ({eval}
     (match args
       (`(,e)  ;; TODO env param
        (sev (exp-parse e) global-env k))))
    ({throw}
     (match args
       (`(,outcome)
        (throw k outcome))))
    ({exit}
     (match args
       (`(,outcome)
        (exit k outcome))))
    ))

(to (apply-primitive p args k)
  ;; Ugh, Squeam's error-catching stuff is pretty clumsy.
  ;; I'm not going to try to handle errors everywhere; only in these
  ;; primitive calls (mostly). We can wait to do things properly until
  ;; we're making a VM in C for real.
  (match (with-signal-handler
          (given (squeam-k @evil)
            (squeam-k .answer {error evil}))
          (given ()
            (call p args)))
    ({error evil}
     (exit k evil))
    (result
     {go k result})))

(to (match-clauses r map clauses datum)
  (begin matching ((clauses clauses))
    (match clauses
      ('() #no)
      (`(,(and clause {clause p e}) ,@rest)
       map.clear!
       (match (match-pat r map p datum)
         (#no (matching rest))
         (#yes clause))))))

(to (match-pats r map ps vals)
  (for every ((`(,p ,val) (zip ps vals))) ;TODO ensure left-to-right order
    (match-pat r map p val)))

(to (match-pat r map p val)
;;  (print `(match-pat ,map ,p ,val))
  (match p
    ({bind name}
     (surely (not (map .maps? name)) "already set")
     (map .set! name val)
     #yes)
    ({ignore}
     #yes)
    ({expect constant}
     (= constant val))
    ({link pf pr}
     (and (cons? val)
          (match-pat r map pf val.first)
          (match-pat r map pr val.rest)))
    ({tuple-pat ps}
     (and (array? val)
          (= ps.count val.count)
          (match-pats r map ps val)))
    ({expect-var name}
     (= (env-get r name) val))
    ))


;; Environments

(to (env-get r name)
  (match r
    ({local-env map parent}
     (match (map .get name)
       (#no (env-get parent name))
       (value value)))
    ({recursive-env map parent}
     (match (map .get name)
       (#no (env-get parent name))
       ({to f params body}
        {closure r params body})))
    ({builtins-env}
     (builtins-map name))
    ))


;; Primitive procedures and the global environment

(to (me)
  the-running-process.^)

(to (! pid message)
  ;; TODO make sure pid is a pid
  (pid .enqueue message)
  #no)

(to (monitor pid)             ;TODO flesh out more of what Erlang does
  (pid .subscribe (me)))

(to (unmonitor pid)
  (pid .unsubscribe (me)))

(to (first x) x.first)
(to (rest x) x.rest)
(let link cons)
(let link? cons?)
(let nil? null?)
(to (list @xs) xs)

(let tuple? array?)
(let tuple<- array<-)

(let prim-tuple<- {primitive tuple<-})

(let primitives-from-squeam
  '(link first rest list chain 
    nil? link? list? number? integer? symbol? claim? char? string? tuple?
    symbol<- char<- tuple<-
    number<-string string<-number list<-string self-evaluating? 
    inexact<-exact exact<-inexact floor not assoc sqrt
    = not= < <=> > <= >= 
    * / + - expt abs gcd
    ! me spawn monitor unmonitor
    module-load   ;; for now
    ;; From Squeam stdlib:
    reverse zip transpose identity format
    count? yeah? min max grid* intercalate sum 
    write print display newline read
    ))

(let builtins-map
  (map<- (for each ((name primitives-from-squeam))
           `(,name {primitive ,(evaluate name '())}))))

(builtins-map .set! 'apply {apply})
(builtins-map .set! 'eval  {eval})
(builtins-map .set! 'throw {throw})
(builtins-map .set! 'exit  {exit})

(to (module-env<- module)
  {recursive-env (map<-defs module) global-env})

(to (map<-defs defs)
  (map<- (for each ((def defs))
           (match def
             ({to name _ _} `(,name ,def))))))

;; Add the prelude to the global environment. This code is almost just
;; calling module-load on prelude.scm, except we can't call that yet,
;; because global-env needs to exist first.
(let global-env
  (hide
    (let defs (with-input-file read-all "eg/squirm/prelude.scm"))
    (let module (module-parse defs))
    {recursive-env (map<-defs module) {builtins-env}}))
