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
  (hey (process<- {go {spawn f}
                      (or arguments '())})
       schedule!))

(to (schedule! process)
  (push! run-queue process))

(to (push! q-box element)
  (q-box .^= (push q-box.^ element)))

(to (running wait-check-in)
  (when (= 0 wait-check-in)
    (check-timeouts))
  (may (peek run-queue.^)
    (be {empty}
      (unless waiting-timeouts.none?
        (let deadline (min @waiting-timeouts.values))
        (nanosleep (- deadline (nano-now)))
        (running 0)))
    (be {nonempty pid q2}
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

(let pid-counter (box<- -1))

(to (process<- start-state)
  (let pid-num (pid-counter .update _.+)) ; Just for display.
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

    (to (_ .selfie sink)
      (format .to-sink sink "#<~w>" pid-num))

    (to (_ .subscribe watcher)
      (watchers .add! watcher))
    (to (_ .unsubscribe watcher)
      (watchers .delete! watcher))

    (to (_ .receive-signal pid outcome)
      (process .enqueue ['DOWN pid outcome]))

    (to (_ .partner pid)
      (partners .add! pid))
    (to (_ .unpartner pid)
      (surely (partners .maps? pid))
      (partners .delete! pid))

    (to (_ .partner-died pid outcome)
      (surely (partners .maps? pid))     ;uh right?
      (partners .delete! pid)
      (may state.^
        (be {exit _})
        (else                              ;TODO check process_flag for trap
          (state .^= {exit 'partner-died}) ;TODO or whatever
          (on-death 'partner-died))))      ;TODO or whatever

    (to (_ .enqueue message)
      ;; TODO: error if exited? Probably not.
      (push! inbox-unchecked message)
      process.wake)

    (to _.wake
      (may state.^
        (be (and {blocked _ _ _ _ _} thunk)
          (schedule! process)
          (state .^= {go thunk #no})) ;(TODO still a bit clumsy)
        (else)))

    (to (_ .receive deadline after-e clauses r k)
      (begin checking ()  ;; TODO finer time-slicing?
        (may (peek inbox-unchecked.^)
          (be {empty}
            (hm (unless deadline
                  {blocked deadline after-e clauses r k})
                (when (<= deadline (nano-now))
;;                (format "time ~w: deadline ~w\n" (msecs (nano-now)) (msecs deadline))
                  (waiting-timeouts .delete! process) ;TODO might not be needed?
                  (sev after-e r k))
                (else
                  (waiting-timeouts .set! process deadline)
                  {blocked deadline after-e clauses r k})))
          (be {nonempty msg rest}
            (inbox-unchecked .^= rest)
            (let map (map<-))
            ;; TODO handle {after ...} clauses
            (may (match-clauses r map clauses msg)
              (be #no
                (push! inbox-checked msg)
                (checking))
              (be {clause _ exp}
                (inbox-unchecked .^= (extend inbox-checked.^ (list<-queue rest)))
                (inbox-checked .^= empty)
                (when deadline
                  (waiting-timeouts .delete! process)) ;TODO might not be needed?
                (sev exp {local-env map r} k)))))))

    (to _.run-a-slice
      (may state.^
        (be {go k value}
          (the-running-process .^= process)
          (let state2 (go k value))
          (the-running-process .^= #no)
          (state .^= state2)
          (may state2
            (be {blocked _ _ _ _ _}
              (surely (empty? inbox-unchecked.^) "inbox populated"))
            (be {exit outcome}
              (on-death outcome))
            (else
              (schedule! process))))
        (be {exit _}
          (surely #no))
        (be {blocked _ _ _ _ _}
          (surely (empty? inbox-unchecked.^) "I'm supposed to be blocked"))
        ))))


;; Modules

(let modules (map<-))

(let autoload-base "eg/squirm/lib/")

(to (module-ref mod-name var-name)
  (may (modules .get mod-name)
    (be #no
      (let filename (chain autoload-base mod-name.name ".scm"))
      (module-load mod-name filename)
      (env-get (modules mod-name)
               var-name))
    (be mod-env
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
  (may def
    (be `(to (,(? symbol? name) ,@params) ,@body)
      {to name (each pat-parse params) (seq-parse body)})
    (be `(to (,(? link? nested) ,@params) ,@body)
      (def-parse `(to ,nested (on ,params ,@body))))
    ))

(to (seq-parse exps)
;;  (print `(seq-parse ,exps))
  (may exps
;; Not sure we want this:
;;    ('()
;;     (exp-parse #no))
    (be `(,e)
      (exp-parse e))
    (be `((let ,p ,e) ,@es)
      (exp-parse `((on (,p) ,@es) ,e)))
    (be `((define ,@defs) ,@es)
      {define (each def-parse defs) (seq-parse es)})
    (be `((to ,@_) ,@es)
      (seq-parse `((define ,exps.first) ,@es)))
    (be `(,e ,@es)
      {then (exp-parse e) (seq-parse es)}))) ;TODO parse as ((let _ ,e) .@es) ?

(to (exp-parse e)
  (may e
    (be (? symbol?)
      (if (module-ref? e)
          (module-ref-parse e)
          {var e}))
    (be (? self-evaluating?)
      {const e})
    (be `',value
      {const value})
    (be (? array?)
      {call {const prim-tuple<-} (each exp-parse e)})
    (be `(on ,ps ,@body)
      {on (each pat-parse ps) (seq-parse body)})
    (be `(if ,t ,y ,n)
      ;; TODO macroexpand to a match expression instead?
      {if (exp-parse t) (exp-parse y) (exp-parse n)})
    (be `(do ,@es)
      (seq-parse es))
    (be `(? ,@clauses)
      ;; TODO less clumsy way to code this?
      (begin collecting ((cs (each clause-parse clauses))
                         (pattern-cs '())
                         (after-c #no))
        (may cs
          (be '()
            {receive (reverse pattern-cs) after-c})
          (be `(,c ,@rest)
            (may c
              (be {after _ _}
                (surely (not after-c) "Multiple after clauses")
                (collecting rest pattern-cs c))
              (be {clause _ _}
                (collecting rest (link c pattern-cs) after-c)))))))
    (be `(be ,subject ,@clauses)
      {be (exp-parse subject) (each clause-parse clauses)})
    (be `(catch ,@es)   ;; TODO macroexpand into (%catch (on () e)) ?
      {catch (seq-parse es)})
    (be `(,operator ,@operands)
      (may (exp-macro-expand operator operands)
        (be #no {call (exp-parse operator) (each exp-parse operands)})
        (be expanded (exp-parse expanded))))))

(to (exp-macro-expand operator operands) ;TODO: make it extensible
  ;; TODO: quasiquote
  (may operator
    (be 'and
      (may operands
        (be '() #no)
        (be `(,e) e)
        (be `(,e ,@es) `(if ,e (and ,@es) #no))))
    (be 'begin
      ;; (begin f ((x a) (y b)) e) => (do (to (f a b) e) (f x y))  ;; TODO tighter scope for f
      (may operands
        (be `(,f ,pairs ,@body)
          (for each! ((pair pairs))
            (surely (and (list? pair) (= pair.count 2))))
          `(do
             (to (,f ,@(each _.first pairs))
               ,@body)
             (f ,@(for each ((pair pairs)) (pair 1)))))))
    (be 'case
      (may operands
        (be '()
          '(exit "No true case")) ;TODO hygiene, & settle on what to exit with
        (be `((else ,@seq))
          `(do ,@seq))
        (be `((,test ,@seq) ,@clauses)
          `(if ,test
               (do ,@seq)
               (case ,@clauses)))))
    (be 'for
      ;;(for f ((x a) (y b)) e) => (f (on (a b) e) x y)
      (may operands
        (be `(,f ,pairs ,@body)
          (for each! ((pair pairs))
            (surely (and (list? pair) (= pair.count 2))))
          `(,f (on ,(each _.first pairs) ,@body)
               ,@(for each ((pair pairs)) (pair 1))))))
    (be 'or
      (may operands
        (be '() #yes)
        (be `(,e) e)
        (be `(,e ,@es)
          `(be ,e
             (#no (or ,@es))
             (yeah yeah)))))
    (be 'quasiquote
      (may operands
        (be `(,sexpr) (qq-expand sexpr))))
    (be 'unless
      (may operands
        (be `(,test ,@body)
          `(be ,test
             (#no ,@body)
             (_ #no)))))
    (be 'when
      (may operands
        (be `(,test ,@body)
          `(be ,test
             (#no #no)
             (_ ,@body)))))
    (else #no)))

;; Expand a quasiquoted expression or pattern (either one).
(to (qq-expand sexpr)
  ;; N.B. unquote-splicing only at the end
  (may sexpr
    (be (list<- 'unquote e)
      e)
    (be (list<- (list<- 'unquote-splicing e))
      e)
    (be (link 'unquote _)          (error "Bad quasiquote"))
    (be (link 'unquote-splicing _) (error "Bad quasiquote"))
    (be (link first rest)
      ;; TODO quote if both parts are constant
      `(link ,(qq-expand first) ;XXX unhygienic but works for both exp and pat
             ,(qq-expand rest)))
    (be (? array?)
      ;; TODO quote if all parts are constant
      (array<-list (each qq-expand sexpr)))
    (else
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
  (may pattern
    (be `(after ,e)
      {after (exp-parse e) (seq-parse seq)})
    (else
      {clause (pat-parse pattern) (seq-parse seq)})))

(to (pat-parse pattern)
  (may pattern
    (be (? symbol?)
      (surely (not (module-ref? pattern)))
      (if (pattern.name .prefix? "_")
          {ignore}
          {bind pattern}))
    (be (? self-evaluating?)
      {expect pattern})
    (be (? array?)
      {tuple-pat (each pat-parse pattern)})
    (be `',value
      {expect value})
    (be `(link ,pf ,pr)
      {link (pat-parse pf) (pat-parse pr)})
    (be `(list ,@ps)
      (pat-parse (if ps.none?
                     ''()
                     `(link ,ps.first (list ,@ps.rest)))))
    (be `(: ,(? symbol? name))
      {expect-var name})
    (be (list<- 'quasiquote sexpr)
      (pat-parse (qq-expand sexpr)))
    ))


;; Interpreter in trampolined style

;; sev: squirm evaluate
;; exp: expression
;; r: environment
;; k: continuation
(to (sev exp r k)
;;  (print `(sev ,exp))
  (may exp
    (be {const value}
      {go k value})
    (be {var name}
      {go k (env-get r name)})           ;TODO error handling
    (be {module-ref mod-name var-name}
      {go k (module-ref mod-name var-name)}) ;XXX module-ref might error or block
    (be {on ps e}
      {go k {closure r ps e}})
    (be {call e es}
      (sev e r {ev-operands es r k}))
    (be {if e _ _}                  ;TODO base on be instead
      (sev e r {branch exp r k}))
    (be {then e1 e2}
      (sev e1 r {then-drop e2 r k}))
    (be {receive clauses after-clause}
      (may after-clause
        (be #no
          (the-running-process.^ .receive #no #no clauses r k))
        (be {after n-exp e}
          (sev n-exp r {receive-timeout e clauses r k}))))
    (be {be e clauses}
      (sev e r {matching clauses r k}))
    (be {catch e}
      (sev e r {catch-frame k}))
    (be {define defs e}
      (sev e {recursive-env (map<-defs defs) r} k))
    ))

(to (go kk value)
;;  (print `(go ,kk ,value))
  (may kk
    (be {ev-operands es r k}
      (ev-operands value '() es r k))
    (be {ev-more-operands f rev-args es r k}
      (ev-operands f (link value rev-args) es r k))
    (be {branch {if _ y n} r k}
      (sev (if value y n) r k))
    (be {then-drop e2 r k}
      (sev e2 r k))
    (be {spawn f}
      (apply f value {halt}))
    (be {blocked deadline after-e clauses r k}
      (the-running-process.^ .receive deadline after-e clauses r k))
    (be {matching clauses r k}
      (let map (map<-))                  ;TODO factor dupe?
      (may (match-clauses r map clauses value)
        (be #no
          (exit k "Match failure"))
        (be {clause _ e}
          (sev e {local-env map r} k))))
    (be {receive-timeout e clauses r k}
      (may value
        (be (? count?)
          (let deadline (+ (nano-now) (* 1000000 value)))
          (the-running-process.^ .receive deadline e clauses r k))))
    (be {catch-frame k}
      {go k value})              ;TODO distinguish from thrown outcome?
    (be {halt}
      {exit 'normal})                    ;TODO design for exit data
    ))

(to (throw kk outcome)
  (may kk                             ;TODO generic walk through k's
    (be {ev-operands es r k}
      (throw k outcome))
    (be {ev-more-operands f rev-args es r k}
      (throw k outcome))
    (be {branch _ _ k}
      (throw k outcome))
    (be {then-drop e2 r k}
      (throw k outcome))
    (be {matching clauses r k}
      (throw k outcome))
    (be {catch-frame k}
      {go k outcome})                    ;TODO distinguish from non-exception result?
    (be {halt}
      {exit outcome})))

(to (exit k reason)
  (throw k ['exit reason]))

(to (ev-operands f rev-args operands r k)
  (may operands
    (be '()
      (apply f (reverse rev-args) k))
    (be `(,e ,@es)
      (sev e r {ev-more-operands f rev-args es r k}))))

(to (apply f args k)
  (may f
    (be {closure r ps e}
      (surely (= args.count ps.count) "arity mismatch")
      (let map (map<-))
      (may (match-pats r map ps args)
        (be #no
          (exit k "Match failure"))
        (be #yes
          (sev e {local-env map r} k))))
    (be {primitive p}
      (apply-primitive p args k))
    (be {apply}
      (may args
        (be `(,f1 ,args1)
          (apply f1 args1 k))))
    (be {eval}
      (may args
        (be `(,e)  ;; TODO env param
          (sev (exp-parse e) global-env k))))
    (be {throw}
      (may args
        (be `(,outcome)
          (throw k outcome))))
    (be {exit}
      (may args
        (be `(,outcome)
          (exit k outcome))))
    ))

(to (apply-primitive p args k)
  ;; Ugh, Cant's error-catching stuff is pretty clumsy.
  ;; I'm not going to try to handle errors everywhere; only in these
  ;; primitive calls (mostly). We can wait to do things properly until
  ;; we're making a VM in C for real.
  (may (with-signal-handler
        (on (cant-k evil)
          ;; XXX wrong if the error occurs within a nested Cant call context.
          ;;   TODO show it can go wrong
          ;;   We need to use an ejector to get this right.
          (cant-k .answer {error evil}))
        (on ()
          (p @args)))
    (be {error evil}
      (exit k evil))
    (be result
      {go k result})))

(to (match-clauses r map clauses datum)
  (begin matching ((clauses clauses))
    (may clauses
      (be '()
        #no)
      (be `(,(and clause {clause p e}) ,@rest)
        map.clear!
        (may (match-pat r map p datum)
          (be #no  (matching rest))
          (be #yes clause))))))

(to (match-pats r map ps vals)
  ;; TODO n-arg every
  (for every (((_ p val) (zip ps vals))) ;TODO ensure left-to-right order
    (match-pat r map p val)))

(to (match-pat r map p val)
;;  (print `(match-pat ,map ,p ,val))
  (may p
    (be {bind name}
      (surely (not (map .maps? name)) "already set")
      (map .set! name val)
      #yes)
    (be {ignore}
      #yes)
    (be {expect constant}
      (= constant val))
    (be {link pf pr}
      (and (link? val)
           (match-pat r map pf val.first)
           (match-pat r map pr val.rest)))
    (be {tuple-pat ps}
      (and (array? val)
           (= ps.count val.count)
           (match-pats r map ps val)))
    (be {expect-var name}
      (= (env-get r name) val))
    ))


;; Environments

(to (env-get r name)
  (may r
    (be {local-env map parent}
      (may (map .get name)
        (be #no   (env-get parent name))
        (be value value)))
    (be {recursive-env map parent}
      (may (map .get name)
        (be #no
          (env-get parent name))
        (be {to f params body}
          {closure r params body})))
    (be {builtins-env}
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
  (to (_ x n)
    (surely (sequence? x))
    (surely (count? n))
    (x .slice n))
  (to (_ x m n)
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

(let primitives-from-cant
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
    reverse zip transpose itself format
    count? yeah? min max grid* intercalate sum sum-by
    write print display newline read
    ;; TODO see if I've changed the stdlib relevantly since collecting these
    ))

(let builtins-map
  (map<-lists (for each ((`(,name ,value) primitives-from-cant.items))
           `(,name {primitive ,value}))))

(builtins-map .set! 'apply {apply})
(builtins-map .set! 'eval  {eval})
(builtins-map .set! 'throw {throw})
(builtins-map .set! 'exit  {exit})

(to (module-env<- module @(optional env))
  {recursive-env (map<-defs module) (or env global-env)})

(to (map<-defs defs)
  (map<-lists (for each ((def defs))
           (may def
             (be {to name _ _} `(,name ,def))))))

;; Add the prelude to the global environment.
(let global-env
  (module-read "eg/squirm/prelude.scm" {builtins-env}))


(export
  run-file)
