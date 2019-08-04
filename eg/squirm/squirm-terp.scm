;; Basic Squirm interpreter as a testable sketch of the semantics

(import (use 'queue)
  empty empty? push peek extend list<-queue)


;; Main

(to (run-file filename @(optional entry arguments))
  (let module (with-input-file read-all filename))
  (run module entry arguments))

(to (run module @(optional entry arguments))
  (squirm-run module (or entry 'main) (or arguments '())))

(to (squirm-run module entry arguments)
  registry.clear!
  (run-queue .^= empty)
  (spawn (env-get (module-env<- (module-parse module)) entry)
         arguments)
  (running wait-check-interval))


;; Processes, scheduling, message passing

(to (spawn f @(optional arguments))
  (let new-process (process<- {go {spawn f}
                                  (or arguments '())}))
  (run-queue .^= (push run-queue.^ new-process))
  new-process)

(to (running wait-check-in)
  (when (= 0 wait-check-in)
    (check-timeouts))
  (be (peek run-queue.^)
    ({empty}
     (unless waiting-timeouts.empty?
       (let deadline (call min waiting-timeouts.values))
       (nanosleep (- deadline (nano-now)))
       (running 0)))
    ({nonempty pid q2}
     (run-queue .^= q2)
     pid.run-a-slice
     (running (- wait-check-in 1)))))

(let wait-check-interval 10)

(let run-queue (box<- empty))
(let the-running-process (box<- #no))
(let waiting-timeouts (map<-))

(to (check-timeouts)
  (let now (nano-now))
  (for each! ((`(,process ,time) waiting-timeouts.items))
;;    (format "time ~w: checking ~w\n" (msecs now) (msecs time))
    (when (<= time now)
      (waiting-timeouts .delete! process)
      ;; TODO: wake it specifically as hitting the timeout, instead of re-evaluating the receive?
      process.wake)))

(let pid-counter (box<- 0))

(to (process<- start-state)
  (let pid-num pid-counter.^)           ; Just for display.
  (pid-counter .^= (+ pid-num 1))
  (let state (box<- start-state))       ; State of execution.
  (let inbox-checked (box<- empty))     ; Messages that didn't match the current receive.
  (let inbox-unchecked (box<- empty))   ; Messages not yet checked against the current receive.
  (let watchers (set<-))                ; Monitors to notify on my exit.
  (let partners (set<-))                ; Partners, whose fate is linked with mine.
  ;; TODO: just one set of a sum type of watchers and partners?

  (to (on-death outcome)
    (for each! ((watcher watchers.keys))
      (watchers .delete! watcher)
      (watcher .receive-signal process outcome)) ;TODO pass more info
    (for each! ((partner partners.keys))
      (partners .delete! partner)
      (partner .partner-died process outcome))) ;TODO pass more info or what?

  (make process

    ({.selfie sink}
     (format .to-sink sink "#<~w>" pid-num))

    ({.subscribe watcher}
     (watchers .add! watcher))
    ({.unsubscribe watcher}
     (watchers .delete! watcher))

    ({.receive-signal pid outcome}
     (process .enqueue (array<- 'DOWN pid outcome)))

    ({.partner pid}
     (partners .add! pid))
    ({.unpartner pid}
     (surely (partners .maps? pid))
     (partners .delete! pid))

    ({.partner-died pid outcome}
     (surely (partners .maps? pid))     ;uh right?
     (partners .delete! pid)
     (be state.^
       ({exit _})
       (_                            ;TODO check process_flag for trap
        (state .^= {exit 'partner-died}) ;TODO or whatever
        (on-death 'partner-died))))      ;TODO or whatever

    ({.enqueue message}
     ;; TODO: error if exited? Probably not.
     (inbox-unchecked .^= (push inbox-unchecked.^ message))
     process.wake)

    ({.wake}
     (be state.^
       ((and {blocked _ _ _ _ _} thunk)
        (run-queue .^= (push run-queue.^ process))
        (state .^= {go thunk #no})) ;(TODO still a bit clumsy)
       (_)))

    ({.receive deadline after-e clauses r k}
     (begin checking ()  ;; TODO finer time-slicing?
       (be (peek inbox-unchecked.^)
         ({empty}
          (if deadline
              (hm (when (<= deadline (nano-now))
;                     (format "time ~w: deadline ~w\n" (msecs (nano-now)) (msecs deadline))
                    (waiting-timeouts .delete! process) ;TODO might not be needed?
                    (sev after-e r k))
                  (else
                    (waiting-timeouts .set! process deadline)
                    {blocked deadline after-e clauses r k}))
              {blocked deadline after-e clauses r k}))
         ({nonempty msg rest}
          (inbox-unchecked .^= rest)
          (let map (map<-))
          ;; TODO handle {after ...} clauses
          (be (match-clauses r map clauses msg)
            (#no
             (inbox-checked .^= (push inbox-checked.^ msg))
             (checking))
            ({clause _ exp}
             (inbox-unchecked .^= (extend inbox-checked.^ (list<-queue rest)))
             (inbox-checked .^= empty)
             (when deadline
               (waiting-timeouts .delete! process)) ;TODO might not be needed?
             (sev exp {local-env map r} k)))))))

    ({.run-a-slice}
     (be state.^
       ({go k value}
        (the-running-process .^= process)
        (let state2 (go k value))
        (the-running-process .^= #no)
        (state .^= state2)
        (be state2
          ({blocked _ _ _ _ _}
           (surely (empty? inbox-unchecked.^) "inbox populated"))
          ({exit outcome}
           (on-death outcome))
          (_ (run-queue .^= (push run-queue.^ process)))))
       ({exit _}
        (surely #no))
       ({blocked _ _ _ _ _}
        (surely (empty? inbox-unchecked.^) "I'm supposed to be blocked"))
       ))))


;; Modules

(let modules (map<-))

(let autoload-base "eg/squirm/lib/")

(to (module-ref mod-name var-name)
  (be (modules .get mod-name)
    (#no
     (let filename (chain autoload-base mod-name.name ".scm"))
     (module-load mod-name filename)
     (env-get (modules mod-name)
              var-name))
    (mod-env 
     (env-get mod-env var-name))))

(to (module-load mod-name filename)
  (surely (not (modules .maps? mod-name))) ;XXX for now
  (modules .set! mod-name (module-read filename global-env)))
  
(to (module-read filename env)
  (let defs (with-input-file read-all filename))  
  (module-env<- (module-parse defs) env))
  

;; Language syntax

(to (module-parse module)
  (each def-parse module))

(to (def-parse def)
;;  (print `(def-parse ,def))
  (be def
    (`(to (,(? symbol? name) ,@params) ,@body)
     {to name (each pat-parse params) (seq-parse body)})
    (`(to (,(? link? nested) ,@params) ,@body)
     (def-parse `(to ,nested (on ,params ,@body))))
    ))

(to (seq-parse exps)
;;  (print `(seq-parse ,exps))
  (be exps
;; Not sure we want this:
;;    ('()
;;     (exp-parse #no))
    (`(,e)
     (exp-parse e))
    (`((let ,p ,e) ,@es)
     (exp-parse `((on (,p) ,@es) ,e)))
    (`((define ,@defs) ,@es)
     {define (each def-parse defs) (seq-parse es)})
    (`((to ,@_) ,@es)
     (seq-parse `((define ,exps.first) ,@es)))
    (`(,e ,@es)
     {then (exp-parse e) (seq-parse es)}))) ;TODO parse as ((let _ ,e) .@es) ?

(to (exp-parse e)
  (be e
    ((? symbol?)
     (if (module-ref? e)
         (module-ref-parse e)
         {var e}))
    ((? self-evaluating?) {const e})
    (`',value             {const value})
    ((? array?)
     {call {const prim-tuple<-} (each exp-parse e)})
    (`(on ,ps ,@body)
     {on (each pat-parse ps) (seq-parse body)})
    (`(if ,t ,y ,n)
     ;; TODO macroexpand to a (be ...) instead?
     {if (exp-parse t) (exp-parse y) (exp-parse n)})
    (`(do ,@es)
     (seq-parse es))
    (`(? ,@clauses)
     ;; TODO less clumsy way to code this?
     (begin collecting ((cs (each clause-parse clauses))
                        (pattern-cs '())
                        (after-c #no))
       (be cs
         ('()
          {receive (reverse pattern-cs) after-c})
         (`(,c ,@rest)
          (be c
            ({after _ _}
             (surely (not after-c) "Multiple after clauses")
             (collecting rest pattern-cs c))
            ({clause _ _}
             (collecting rest (link c pattern-cs) after-c)))))))
    (`(be ,subject ,@clauses)
     {be (exp-parse subject) (each clause-parse clauses)})
    (`(catch ,@es)   ;; TODO macroexpand into (%catch (on () e)) ?
     {catch (seq-parse es)})
    (`(,operator ,@operands)
     (be (exp-macro-expand operator operands)
       (#no {call (exp-parse operator) (each exp-parse operands)})
       (expanded (exp-parse expanded))))))

(to (exp-macro-expand operator operands) ;TODO: make it extensible
  ;; TODO: quasiquote
  (be operator
    ('and
     (be operands
       ('() #no)
       (`(,e) e)
       (`(,e ,@es) `(if ,e (and ,@es) #no))))
    ('begin
        ;; (begin f ((x a) (y b)) e) => (do (to (f a b) e) (f x y))  ;; TODO tighter scope for f
     (be operands
       (`(,f ,pairs ,@body)
        (for each! ((pair pairs))
          (surely (and (list? pair) (= pair.count 2))))
        `(do
           (to (,f ,@(each '.first pairs))
             ,@body)
           (f ,@(for each ((pair pairs)) (pair 1)))))))
    ('case
     (be operands
       ('()
        '(exit "No true case")) ;TODO hygiene, & settle on what to exit with
       (`((else ,@seq))
        `(do ,@seq))
       (`((,test ,@seq) ,@clauses)
        `(if ,test
             (do ,@seq)
             (case ,@clauses)))))
    ('for
        ;;(for f ((x a) (y b)) e) => (f (on (a b) e) x y)
     (be operands
       (`(,f ,pairs ,@body)
        (for each! ((pair pairs))
          (surely (and (list? pair) (= pair.count 2))))
        `(,f (on ,(each '.first pairs) ,@body)
             ,@(for each ((pair pairs)) (pair 1))))))
    ('or
     (be operands
       ('() #yes)
       (`(,e) e)
       (`(,e ,@es)
        `(be ,e
           (#no (or ,@es))
           (yeah yeah)))))
    ('quasiquote
     (be operands
       (`(,sexpr) (qq-expand sexpr))))
    ('unless
     (be operands
       (`(,test ,@body)
        `(be ,test
           (#no ,@body)
           (_ #no)))))
    ('when
     (be operands
       (`(,test ,@body)
        `(be ,test
           (#no #no)
           (_ ,@body)))))
    (_ #no)))

;; Expand a quasiquoted expression or pattern (either one).
(to (qq-expand sexpr)
  ;; N.B. unquote-splicing only at the end
  (be sexpr
    ((list<- 'unquote e)
     e)
    ((list<- (list<- 'unquote-splicing e))
     e)
    ((link 'unquote _)          (error "Bad quasiquote"))
    ((link 'unquote-splicing _) (error "Bad quasiquote"))
    ((link first rest)
     ;; TODO quote if both parts are constant
     `(link ,(qq-expand first) ;XXX unhygienic but works for both exp and pat
            ,(qq-expand rest)))
    ((? array?)
     ;; TODO quote if all parts are constant
     (array<-list (each qq-expand sexpr)))
    (_
     `',sexpr)))

;; Transform foo:bar to {module-ref foo bar}
;; (TODO better to do this in the reader in the real system.)
(to (module-ref? symbol)
  (symbol.name .find? #\:))

(to (module-ref-parse symbol)
  (let `(,mod ,var) (symbol.name .split ":"))
  {module-ref (symbol<- mod) (symbol<- var)})

(to (clause-parse clause)
  (let `(,pattern ,@seq) clause)
  (be pattern
    (`(after ,e)
     {after (exp-parse e) (seq-parse seq)})
    (_
     {clause (pat-parse pattern) (seq-parse seq)})))

(to (pat-parse pattern)
  (be pattern
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
    ((list<- 'quasiquote sexpr)
     (pat-parse (qq-expand sexpr)))
    ))


;; Interpreter in trampolined style

;; sev: squirm evaluate
;; exp: expression
;; r: environment
;; k: continuation
(to (sev exp r k)
;;  (print `(sev ,exp))
  (be exp
    ({const value}
     {go k value})
    ({var name}
     {go k (env-get r name)})           ;TODO error handling
    ({module-ref mod-name var-name}
     {go k (module-ref mod-name var-name)}) ;XXX module-ref might error or block
    ({on ps e}
     {go k {closure r ps e}})
    ({call e es}
     (sev e r {ev-operands es r k}))
    ({if e _ _}                  ;TODO base on be instead
     (sev e r {branch exp r k}))
    ({then e1 e2}
     (sev e1 r {then-drop e2 r k}))
    ({receive clauses after-clause}
     (be after-clause
       (#no
        (the-running-process.^ .receive #no #no clauses r k))
       ({after n-exp e}
        (sev n-exp r {receive-timeout e clauses r k}))))
    ({be e clauses}
     (sev e r {matching clauses r k}))
    ({catch e}
     (sev e r {catch-frame k}))
    ({define defs e}
     (sev e {recursive-env (map<-defs defs) r} k))
    ))

(to (go kk value)
;;  (print `(go ,kk ,value))
  (be kk
    ({ev-operands es r k}
     (ev-operands value '() es r k))
    ({ev-more-operands f rev-args es r k}
     (ev-operands f (link value rev-args) es r k))
    ({branch {if _ y n} r k}
     (sev (if value y n) r k))
    ({then-drop e2 r k}
     (sev e2 r k))
    ({spawn f}
     (apply f value {halt}))
    ({blocked deadline after-e clauses r k}
     (the-running-process.^ .receive deadline after-e clauses r k))
    ({matching clauses r k}
     (let map (map<-))                  ;TODO factor dupe?
     (be (match-clauses r map clauses value)
       (#no (exit "Match failure"))
       ({clause _ e}
        (sev e {local-env map r} k))))
    ({receive-timeout e clauses r k}
     (be value
       ((? count?)
        (let deadline (+ (nano-now) (* 1000000 value)))
        (the-running-process.^ .receive deadline e clauses r k))))
    ({catch-frame k}
     {go k value})              ;TODO distinguish from thrown outcome?
    ({halt}
     {exit 'normal})                    ;TODO design for exit data
    ))

(to (throw kk outcome)
  (be kk                             ;TODO generic walk through k's
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
  (be operands
    ('()
     (apply f (reverse rev-args) k))
    (`(,e ,@es)
     (sev e r {ev-more-operands f rev-args es r k}))))

(to (apply f args k)
  (be f
    ({closure r ps e}
     (surely (= args.count ps.count) "arity mismatch")
     (let map (map<-))
     (be (match-pats r map ps args)
       (#no
        (exit k "Match failure"))
       (#yes
        (sev e {local-env map r} k))))
    ({primitive p}
     (apply-primitive p args k))
    ({apply}
     (be args
       (`(,f1 ,args1)
        (apply f1 args1 k))))
    ({eval}
     (be args
       (`(,e)  ;; TODO env param
        (sev (exp-parse e) global-env k))))
    ({throw}
     (be args
       (`(,outcome)
        (throw k outcome))))
    ({exit}
     (be args
       (`(,outcome)
        (exit k outcome))))
    ))

(to (apply-primitive p args k)
  ;; Ugh, Squeam's error-catching stuff is pretty clumsy.
  ;; I'm not going to try to handle errors everywhere; only in these
  ;; primitive calls (mostly). We can wait to do things properly until
  ;; we're making a VM in C for real.
  (be (with-signal-handler
        (on (squeam-k @evil)
          (squeam-k .answer {error evil}))
        (on ()
          (call p args)))
    ({error evil}
     (exit k evil))
    (result
     {go k result})))

(to (match-clauses r map clauses datum)
  (begin matching ((clauses clauses))
    (be clauses
      ('() #no)
      (`(,(and clause {clause p e}) ,@rest)
       map.clear!
       (be (match-pat r map p datum)
         (#no (matching rest))
         (#yes clause))))))

(to (match-pats r map ps vals)
  (for every ((`(,p ,val) (zip ps vals))) ;TODO ensure left-to-right order
    (match-pat r map p val)))

(to (match-pat r map p val)
;;  (print `(match-pat ,map ,p ,val))
  (be p
    ({bind name}
     (surely (not (map .maps? name)) "already set")
     (map .set! name val)
     #yes)
    ({ignore}
     #yes)
    ({expect constant}
     (= constant val))
    ({link pf pr}
     (and (link? val)
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
  (be r
    ({local-env map parent}
     (be (map .get name)
       (#no (env-get parent name))
       (value value)))
    ({recursive-env map parent}
     (be (map .get name)
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
  ((as-pid pid) .enqueue message)
  #no)

(to (as-pid pid)
  ;; TODO error if not a pid in the end
  (if (symbol? pid) (registry pid) pid))

(let registry (map<-))

(to (register name pid)
  (registry .set! name (as-pid pid)) ;; TODO is the as-pid a good idea?
  ;; TODO what if already set?
  ;; TODO unregister when it dies?
  pid)

(to (unregister name)
  (surely (registry .maps? name))
  (registry .delete! name))

(to (monitor pid)             ;TODO flesh out more of what Erlang does
  ((as-pid pid) .subscribe (me)))

(to (unmonitor pid)
  ((as-pid pid) .unsubscribe (me)))

(to (partner pid)
  (let m (me))
  (let p (as-pid pid))
  (m .partner pid)
  (pid .partner m)
  #no)                                  ;TODO what return value?

(to (unpartner pid)
  (let m (me))
  (let p (as-pid pid))
  (m .unpartner pid)
  (pid .unpartner m)
  #no)                                  ;TODO what return value?

(to (spawn-partner f @(optional arguments))
  (let pid (spawn f arguments))
  (partner pid)
  pid)

(to (first x) x.first)
(to (rest x) x.rest)
(let nil? null?)
(to (list @xs) xs)
(to (length x) x.count)
(to (nth x n)                           ;TODO better name?
  (surely (sequence? x))
  (surely (count? n))
  (x n))
(make slice
  (`(,x ,n)
   (surely (sequence? x))
   (surely (count? n))
   (x .slice n))
  (`(,x ,m ,n)
   (surely (sequence? x))
   (surely (count? m))
   (surely (count? n))
   (x .slice m n)))

(to (sequence? x) (or (array? x) (string? x) (list? x)))

(let tuple? array?)
(let tuple<- array<-)
(let tuple<-list array<-list)

(let prim-tuple<- {primitive tuple<-})

(to (quotient n d)  (n .quotient d))
(to (remainder n d) (n .remainder d))
(to (modulo n d)    (n .modulo d))

(to (string<-symbol sym)
  (surely (symbol? sym))
  sym.name)

(let primitives-from-squeam
  (export
    link first rest list chain length nth slice
    nil? link? list? number? integer? symbol? claim? char? string? tuple?
    symbol<- char<- tuple<- tuple<-list
    number<-string string<-number list<-string string<-list self-evaluating? 
    inexact<-exact exact<-inexact floor not assoc sqrt
    = not= < <=> > <= >= 
    * / + - expt abs gcd quotient remainder modulo
    string<-symbol
    ! me spawn monitor unmonitor partner unpartner spawn-partner
    register unregister
    module-load   ;; for now
    reverse zip transpose identity format
    count? yeah? min max grid* intercalate sum 
    write print display newline read
    ))

(let builtins-map
  (map<- (for each ((`(,name ,value) primitives-from-squeam.items))
           `(,name {primitive ,value}))))

(builtins-map .set! 'apply {apply})
(builtins-map .set! 'eval  {eval})
(builtins-map .set! 'throw {throw})
(builtins-map .set! 'exit  {exit})

(to (module-env<- module @(optional env))
  {recursive-env (map<-defs module) (or env global-env)})

(to (map<-defs defs)
  (map<- (for each ((def defs))
           (be def
             ({to name _ _} `(,name ,def))))))

;; Add the prelude to the global environment.
(let global-env
  (module-read "eg/squirm/prelude.scm" {builtins-env}))


(export
  run-file)
