;; a la The Little Prover
;; #yes/#no for 't/'nil
;; Renamed some operators: atom? nat? =
;; I'm adding an assumption that arguments are well-formed or well-typed. (to be fleshed out)
;; Going to assume things are pre-parsed into structs
;;   (I'm not sure that's a net win for readability)
;; Factored def structs together

;; More notes & nits about Appendix C:
;; * They have a forward ref to arity? contradicting their claim to bottom-up style
;; * Mis-grouping of code and literate-doc blocks
;; * The prelude seems to omit the rators -- don't you need those defs?
;; * I don't think the book ever explicitly said just what you can conclude from the output.
;;   E.g. can you feed in an absurdity as an axiom?
;;   Maybe it's: given a standard set of axioms, running a certain standard interpreter
;;   always respects any theorems proved by Bob.
;; * exprs? and expr? could be better factored?
;;   Actually the reason must be to avoid mutual recursion.
;;   How much complication would it take to support mutual recursion?
;; * Could maybe exploit immutable maps nicely, but they'll need to respect ordering of keys
;; * It'd be interesting to know what's been proved about the book's J-Bob.
;; * With errors generally turning into some boolean failure for the whole computation,
;;   the cause can be a pain to track down.
;; * I think it'd be easier to understand a J-Bob written in stages:
;;   - Just parsing, checking well-formedness.
;;   - Only reason about if-expressions and equality. (I'm not sure if this piece breaks off cleanly.)
;;   - Prove theorems about straight-line code using operators and defuns.
;;   - Handle induction.
;;   - (Beyond J-Bob:) Automate the easy proof steps.
;; * J-Bob's design is a little funny: the axioms for equality and if are just data to it,
;;   as though optional, but there is some logic wired in about them (equality/def, follow-prems).
;;   Might we simplify any code if we wired in those axioms, too? Or OTOH would anything work if
;;   we changed them?
;; * What if we made J-Bob Lisp more like Squeam: pattern matching instead of if, car, cdr?
;;   We'd have to handle local variables (not just parameters), I guess. Would it be worth it?
;; * Might be nice to have a kind of Pre-Scheme-ish thing that expands out higher-order functions
;;   and I guess mutual recursion. You'd want it to instantiate theorems at the same time.
;; * I don't like 'E' as the tag for an if's else-branch, because
;;   there's also 'e' as the abbrev for an expression.

(import (use "lib/pretty-print") pp)    ;XXX for debugging


;; Types:

;; proof:
;;   {proof def seed steps}
;; where seed is an e or #no            XXX make that {constant #no}?

;; def:
;;   {def name formals meaning}
;; meaning:
;;   {fun e}
;;   {thm e}
;;   {rator}          TODO make it {rator fn} to call fn for constant args

;; e: 
;;   {constant datum}
;;   {variable name}
;;   {if e e e}
;;   {call name es}


;; Top level.
;; TODO parse/unparse

(to (bob/define defs proofs)
  (if (valid? defs proofs)
      (rewrite/define+ defs proofs)
      defs))

(to (bob/prove defs proofs)
  (if (valid? defs proofs)
      (rewrite/prove+ defs proofs)
      no-c))

(to (valid? defs proofs)
  (and (defs? '() defs)
       (proofs? defs proofs)))

(to (bob/step defs e steps)
  (if (and (defs? '() defs)
           (expr? defs 'any e)
           (steps? defs steps))
      (rewrite/steps defs e steps)
      e))

;;XXX
(to (trace fn @args)
  (when loud? (pp `(>>> ,fn ,@args)))
  (let result (call fn args))
  (when loud? (pp `(<<< ,fn : ,result)))
  result)

(let loud? #no)


;; TODO explain me
;; TODO did I refactor duplication correctly?
;;   Seems unlikely since it got so much shorter.

(to (rewrite/define+ defs proofs)
  (match proofs
    ('() defs)
    (`({proof ,def ,seed ,steps} ,@rest-proofs)
     (if (= yes-c (rewrite/prove defs def seed steps))
         (rewrite/define+ (append defs def) rest-proofs)
         defs))))


;; TODO explain me

(to (rewrite/prove+ defs proofs)
  (match proofs
    ('() yes-c)
    (`({proof ,def ,seed ,steps} ,@rest-proofs)
     (let e (rewrite/prove+ (append defs def) rest-proofs))
     (if (= e yes-c)
         (rewrite/prove defs def seed steps) ;y'know, if we did this first it'd be more like rewrite/define+
         e))))

(to (rewrite/prove defs def seed steps)
  (let {def _ _ meaning} def)
  (match meaning
    ({fun _} (rewrite/steps defs (totality/claim seed def)       steps))
    ({thm _} (rewrite/steps defs (induction/claim defs seed def) steps))
    (_       no-c)))                    ;TODO just #no?


;; Rewriting... TODO explain me

(to (rewrite/steps defs claim steps)
  (if steps.empty?
      claim
      (begin stepping ((`(,first ,@rest) steps)
                       (old claim))
        (let new (rewrite/step defs old first))
        (if (or (= old new) rest.empty?)
            new
            (stepping rest new)))))

(to (rewrite/step defs claim {step path app})
  (let {call f _} app)
  (equality/def claim path app (lookup f defs)))


;; Equality... TODO explain me
;; N.B. their 'claim' is not a Squeam claim type

(to (equality/def claim path app {def name formals meaning})
  (when loud?
    (pp `(equality/def ,(unparse-e claim) ,path ,(unparse-e app) ,(unparse-def {def name formals meaning}))))
  (let {call _ args} app)
  (let result
    (equality/path claim path
                 (match meaning
                   ;; TODO do these constructed (= foo bar) exprs ever cause rewriting-in-reverse?
                   ;;   Or is this just a fancy way of substituting (eval-op app), etc.?
                   ({rator}    `{call = (,app ,(eval-op app))})
                   ({fun body} (sub-e formals args
                                      `{call = ({call ,name ,(express-variables formals)}
                                                ,body)}))
                   ({thm body} (sub-e formals args body)))))
  (when loud? (pp `(equality/def : ,(unparse-e result))))
  result)

(to (express-variables names)              ;TODO this is clumsy
  (for each ((name names))
    {variable name}))

;; Rewrite the subexpression of e at path according to thm, if possible.
;; thm must be an equality perhaps nested in if-then-elses.
(to (equality/path e path thm)
  (if (focus-is-at-path? path e)        ;N.B. reverse order of args
      (set-at-path path e
                   (trace equality/equation (get-at-path path e)
                                      (follow-prems path e thm)))
      e))

;; Rewrite focus according to concl-inst if concl-inst is an equality.
(to (equality/equation focus concl-inst)
  (match concl-inst
    ({call '= `(,arg1 ,arg2)} (equality focus arg1 arg2))
    (_                        focus)))

;; Taking "a=b" as a rewrite rule, rewrite focus if possible.
(to (equality focus a b)
  (case ((= focus a) b)
        ((= focus b) a)
        (else        focus)))


;; "Check the premises against the instantiated conclusion." (?)
;; thm must be an equality perhaps nested in if-then-elses.
;; If possible, dig the equality out of the if-then-elses.
;; It's possible when every if-question appears also as the question of
;; some if in path through e1.
(to (follow-prems path e1 thm)          ;TODO rename e1?
  (match thm
    ({if q a e}
     (case ((prem-match? 'A q path e1) (follow-prems path e1 a))
           ((prem-match? 'E q path e1) (follow-prems path e1 e))
           (else                       thm)))
    (_ thm)))

;; Somewhere along path through e, does some if have a question equal to
;; prem, followed by dir as the next step in the path?
;; TODO more efficient to check that the next step is either 'A or 'E
;;      and return that, or #no. Then we wouldn't have to call this twice.
(to (prem-match? dir prem path e)
  (begin matching ((path path) (e e))
    (and (not path.empty?)
         (or (and (= path.first dir)
                  (match e
                    ({if q _ _} (= q prem))
                    (_          #no)))
             (matching path.rest (get-at-direction path.first e))))))


;; Rewriting.
;; TODO ought to be shorter

(to (set-at-path path e1 e2)
  (if path.empty?
      e2
      (set-at-direction path.first e1
                        (set-at-path path.rest (get-at-direction path.first e1)
                                     e2))))

(to (get-at-path path e)                ;TODO: foldl
  (if path.empty?
      e
      (get-at-path path.rest (get-at-direction path.first e))))

(to (get-at-direction dir e)
  (match dir
    ('Q       (match e ({if q _ _}    q)))
    ('A       (match e ({if _ a _}    a)))
    ('E       (match e ({if _ _ e}    e)))
    ((? nat?) (match e ({call _ args} (get-arg dir args))))))

(to (set-at-direction dir e1 e2)
  (match dir
    ('Q       (match e1 ({if q a e}    {if e2 a e})))
    ('A       (match e1 ({if q a e}    {if q e2 e})))
    ('E       (match e1 ({if q a e}    {if q a e2})))
    ((? nat?) (match e1 ({call f args} {call f (set-arg dir args e2)})))))

(to (get-arg n args)
  (args (- n 1)))

(to (set-arg n args e)
  (if (= n 1)
      (cons e args.rest)
      (cons args.first (set-arg (- n 1) args.rest e))))


;; Check okayness of a path.

(to (focus-is-at-path? path e)
  (or path.empty?
      (and (focus-is-at-direction? path.first e)
           (focus-is-at-path? path.rest
                              (get-at-direction path.first e)))))
  
(to (focus-is-at-direction? dir e)
  (match dir
    ('Q       (match e ({if _ _ _}    #yes) (_ #no)))
    ('A       (match e ({if _ _ _}    #yes) (_ #no)))
    ('E       (match e ({if _ _ _}    #yes) (_ #no)))
    ((? nat?) (match e ({call _ args} (<= 1 dir args.count))
                       (_ #no)))))


;; Build a totality claim.

(to (totality/claim meas {def name formals {fun body}})
  (if (not meas)                        ;XXX might need to unparse a constant
      {constant ((calls-to name body) .empty?)}
      {if {call 'nat? `(,meas)}
          (totality/if meas name formals body)
          no-c}))

(to (totality/if meas f formals body)
  (match body
    ({if q a e}
     (conjoin (append (totality/meas meas formals (calls-to f q))
                      (if-c-when-necessary q
                                           (totality/if meas f formals a)
                                           (totality/if meas f formals e)))))
    (_ (conjoin (totality/meas meas formals (calls-to f body))))))

(to (totality/meas meas formals calls)
  (for each (({call _ args} calls))
    {call '< `(,(sub-e formals args meas)
               ,meas)}))


;; Build an induction claim.

(to (induction/claim defs seed {def _ _ {thm body}})
  (match seed
    (#no           body)
    ({call f args} (induction/defun args body (lookup f defs)))))

(to (induction/defun vars claim {def f formals {fun body}})
  (induction/if vars claim f (sub-e formals vars body)))

(to (induction/if vars claim f body)
  (match body
    ({if q a e}
     (implies (induction/prems vars claim (calls-to f q))
              (if-c-when-necessary q
                                   (induction/if vars claim f a)
                                   (induction/if vars claim f e))))
    (_ (implies (induction/prems vars claim (calls-to f body))
                claim))))

(to (induction/prems vars claim calls)
  (for each (({call _ args} calls))
    (sub-e vars args claim)))


;; Collect all the calls to f from any subexpression.

(to (calls-to f e)
  (match e
    ({constant _}  '())
    ({variable _}  '())
    ({if q a e}    (exprs-calls-to f `(,q ,a ,e)))
    ({call g args} (list-union (if (= f g) `(,e) '())
                               (exprs-calls-to f args)))))

(to (exprs-calls-to f es)
  (for foldr ((e es) (recs '()))
    (list-union (calls-to f e) recs)))


;; Substitute vars -> args in an expression.

(to (sub-e vars args e)
  (match e
    ({constant _}    e)
    ({variable name} (sub-var vars args name))
    ({if q a e}      {if (sub-e vars args q)
                         (sub-e vars args a)
                         (sub-e vars args e)})
    ({call f es}     {call f (sub-es vars args es)})))

(to (sub-es vars args es)
  (for each ((e es))
    (sub-e vars args e)))

(to (sub-var vars args name)
  (case (vars.empty?         {variable name})
        ((= vars.first name) args.first)
        (else                (sub-var vars.rest args.rest name))))


;; Check okayness of proofs.
;; Each purports to prove a defun or dethm in the context of defs plus
;; the defs of the preceding proofs.
;; A proof is okay when its def, seed, and steps are all okay.
;; XXX the step-checking seems to have no connection to the seed

(to (proofs? defs proofs)
  (or proofs.empty?
      (and (proof? defs proofs.first)
           (do (let {proof def _ _} proofs.first)
               (proofs? (append defs def) proofs.rest)))))

(to (proof? defs {proof def seed steps})
  (and (def? defs def)
       (and seed (seed? defs def seed))
       (steps? (extend-rec defs def) steps))) ;TODO explain why extend-rec is the right thing


;; Check okayness of an induction scheme.
;; A defun's seed can be any okay expression. (It's the measure for a totality proof, right?)
;; A dethm's seed is okay when:
;;  * It's a call to a defun. XXX should it have to be a *recursive* call?
;;  * The args match the target's arity, i.e. it's an okay call.
;;    XXX Actually could it be not okay because the vars are not bound? Should we check?
;;        No, I think we do check with subset? -- vars are the scope at the call.
;;  * The args are all variables, a set, and a subset of the target's formals.
;;    I think this means they're a permutation of the formals.
;;    XXX wtf is all this?
;; Probably my concerns don't come up because seeds come only from totality/claim or induction/claim.
;; I probably need to understand those to understand the checks that we do here.

(to (seed? defs {def _ formals meaning} seed)
  (match meaning
    ({fun _} (expr? defs formals seed))
    ({thm _} (induction-scheme? defs formals seed))
    (_       #no)))          ;does this come up?

(to (induction-scheme? defs vars e)
  (match e
    ({call f args} (induction-scheme-for? (lookup f defs) vars e))
    (_             #no)))

(to (induction-scheme-for? def vars {call _ args})
  (match def
    ({def _ formals {fun _}}
     (and (arity? formals args)
          (formals-exprs? args)
          (subset? args vars)))           ;XXX need to convert exprs/names
    (_ #no)))

(to (formals-exprs? es)                       ;XXX sometimes called with symbols instead of exprs
  (and (every variable? es)
       (set? es)))


;; Check okayness of defs.
;; Each must be okay in the context of just the known and preceding
;; defs. A dethm/defun is okay when it's being defined for the first
;; time and its body is an okay expression. A defun's body may recurse,
;; but not a dethm's.

(to (defs? known-defs defs)
  (or defs.empty?
      (and (def? known-defs defs.first)
           (defs? (append known-defs defs.first) defs.rest))))

(to (def? known-defs def)
  (let {def name formals meaning} def)
  (match meaning
    ({thm body} (and (undefined? name known-defs)
                     (expr? known-defs formals body))) ;TODO y'know, this could use extend-rec too
    ({fun body} (and (undefined? name known-defs)
                     (expr? (extend-rec known-defs def) formals body)))
    ({rator}    #yes)))               ;XXX different from the book.
;;    (_          #no)))                ;TODO needed? or should be #yes?

(to (extend-rec defs {def name formals meaning})
  (match meaning
    ({fun body}
     (append defs {def name formals
                    {fun {call name (express-variables formals)}}})) ;TODO explain why
    (_ defs)))


;; Check okayness of steps.
;; Each step's call must name a def with the correct arity;
;; if it's an operator, the arguments must be constants;
;; else they must be okay expressions.

(to (steps? defs steps)
  (for every ((step steps))
    (let {step _ {call name args}} step)
    (let {def _ formals meaning} (lookup name defs))
    (and (arity? formals args)
         (match meaning
           ({rator} (every constant? args))
           (_       (exprs? defs 'any args)))))) ;XXX so where do we check the boundness of args?


;; Check okayness of expressions.
;; Variables must be bound according to vars;
;; calls must name a defun or operator with the correct arity.

(to (expr? defs vars e)
  (match e
    ({constant _}    #yes)
    ({variable name} (bound? name vars))
    ({if q a e}      (exprs? defs vars `(,q ,a ,e)))
    ({call _ es}     (and (call-arity? defs e)
                          (exprs? defs vars es)))))

(to (exprs? defs vars es)
  (for every ((e es))
    (expr? defs vars e)))

(to (call-arity? defs {call name args})
  (match (lookup name defs)
    ({def _ formals {rator}} (arity? formals args))
    ({def _ formals {fun _}} (arity? formals args))
    (_                       #no))) ;TODO maybe assume this case never happens. Then just ignore the meaning field.

(to (arity? formals args)
  (= formals.count args.count))


;; Name lookup.

(to (lookup name defs)
  (match defs
    ('() #no)                ;N.B. original returns name instead
    (`(,def ,@rest)
     (let {def dname _ _} def)
     (if (= name dname)
         def
         (lookup name rest)))))

(to (undefined? name defs)
  (not (lookup name defs)))

(to (bound? var vars)
  (or (= vars 'any)
      (vars .find? var)))


;; Some expression helpers.

(let yes-c {constant #yes})
(let no-c  {constant #no})

(to (constant? e)
  (match e
    ({constant _} #yes)
    (_            #no)))

(to (variable? e)
  (match e
    ({variable _} #yes)
    (_            #no)))

(to (if-c-when-necessary q a e)
  (if (= a e)
      a
      {if q a e}))

(to (conjoin es)
  (match es
    ('()         yes-c)
    (`(,e)       e)
    (`(,e ,@es1) {if e (conjoin es1) no-c})))

(to (implies es e)
  (match es
    ('()          e)
    (`(,e1 ,@es1) {if e1 (implies es1 e) yes-c})))


;; Apply an operator to values.

(to (eval-op {call f args})
  {constant (apply-op f (for each (({constant value} args))
                          value))})

(to (apply-op rator rands)
  (match rands.count
    (1 (unary-op rator (rands 0)))
    (2 (binary-op rator (rands 0) (rands 1)))))

(to (unary-op rator rand)
  (match rator
    ('atom? (not (cons? rand)))          ;XXX right?
    ('car   (and (cons? rand) rand.first))
    ('cdr   (and (cons? rand) rand.rest))
    ('nat?  (nat? rand))
    ('size  (size rand))))

(to (binary-op rator rand1 rand2)
  (match rator
    ('=    (= rand1 rand2))
    ('cons (cons rand1 rand2))
    ('+    (bob+ rand1 rand2))
    ('<    (bob< rand1 rand2))))


;; Runtime.

(to (nat? x)
  (and (number? x) (integer? x) (<= 0 x)))

(to (size x)
  (match x
    (`(,h ,@t) (+ (size h) (size t)))
    (_ 1)))

(to (bob+ x y)
  (and (number? x) (number? y) (+ x y)))
      
(to (bob< x y)
  (and (number? x) (number? y) (< x y)))


;; Unparse.

(to (unparse-def {def name formals meaning})
  (match meaning
    ({fun body} `(defun ,name ,formals ,(unparse-e body)))
    ({rator}    `(defrator ,name ,formals))
    ({thm body} `(dethm ,name ,formals ,(unparse-e body)))))

(to (unparse-e e)
  (match e
    ({constant datum} `',datum)
    ({variable name}  name)
    ({if q a e}       `(if ,(unparse-e q)
                           ,(unparse-e a)
                           ,(unparse-e e)))
    ({call name es}   `(,name ,@(each unparse-e es)))))


;; Parse.

(to (parse-proof xproof)
  (match xproof
    (`(,xdef ,xseed ,@xsteps)
     {proof (parse-def xdef)
            (and xseed (parse-e xseed))
            (parse-steps xsteps)})))

(to (parse-steps xsteps)
  (surely (list? xsteps) "Steps syntax")
  (each parse-step xsteps))

(to (parse-step xstep)
  (match xstep
    (`(,xpath ,xe)
     ;; TODO check path well-formed
     ;; TODO check that xe is a call? I think?
     {step xpath (parse-e xe)})))

(to (parse-def xdef)
  (match xdef
    (`(defun ,(? symbol? name) ,(? formals? formals) ,body)
     {def name formals {fun (parse-e body)}})
    (`(dethm ,(? symbol? name) ,(? formals? formals) ,body)
     {def name formals {thm (parse-e body)}})
    (`(,(? symbol? name) ,(? formals? formals))
     {def name formals {rator}})))

(to (formals? x)
  (and (list? x)
       (every symbol? x)
       (set? x)))

(to (parse-e xe)
  (match xe
    ((? number?)     {constant xe})
    ((? claim?)      {constant xe})
    (''nil           no-c)
    (''t             yes-c)
    (`',datum        {constant datum})
    ((? symbol?)     {variable xe})
    (`(if ,x1 ,x2 ,x3) {if (parse-e x1) (parse-e x2) (parse-e x3)})
    (`(,f ,@xargs)
     (surely (symbol? f) "Bob syntax")
     {call f (each parse-e xargs)})))


;; Lists as sets with order.

(to (append xs x)
  (if (xs .find? x)
      xs
      (chain xs `(,x))))

(to (list-union xs ys)
  (for foldl ((xs xs) (y ys))
    (append xs y)))

(to (subset? xs ys)
  (for every ((x xs))
    (ys .find? x)))

(to (set? xs)
  (= xs.count ((call set<- xs) .count)))


;; Prelude

(let x-rators
  '(
    (atom? (x))
    (car (x))
    (cdr (x))
    (nat? (x))
    (size (x))
    (= (x y))
    (cons (x y))
    (+ (x y))
    (< (x y))
    ))

(let x-axioms
  '(
    (dethm equal-same (x)
      (= (= x x) #yes))

    (dethm equal-swap (x y)
      (= (= x y) (= y x)))

    (dethm equal-if (x y)
      (if (= x y)
          (= x y)
          #yes))

    (dethm if-true (x y)
      (= (if #yes x y) x))

    (dethm if-false (x y)
      (= (if #no x y) y))

    (dethm if-same (x y)
      (= (if x y y) y))

    (dethm if-nest-A (x y z)
      (if x
          (= (if x y z) y)
          #yes))

    (dethm if-nest-E (x y z)
      (if x
          #yes
          (= (if x y z) z)))

    (dethm atom/cons (x y)
      (= (atom? (cons x y)) #no))

    (dethm car/cons (x y)
      (= (car (cons x y)) x))

    (dethm cdr/cons (x y)
      (= (cdr (cons x y)) y))

    (dethm cons/car+cdr (x)
      (if (atom? x)
          #yes
          (= (cons (car x) (cdr x)) x)))

    ;; TODO more
    ))

(let prelude
  ;; TODO more
  (each parse-def (chain x-rators x-axioms)))


;; Smoke test

(print (set? '(a b c)))
(print (set? '(a b c b)))

(print (subset? '(a b c) '(c a)))
(print (subset? '(c a) '(a b c)))

(print (append '(a b c) 'a))
(print (append '(a b c) 'd))

(print (list-union '(a b c) '(c d b e)))

(print (unparse-e (eval-op (parse-e '(+ 2 3)))))
(print (unparse-e (eval-op (parse-e '(size '(2 3))))))

(print (unparse-e (if-c-when-necessary (parse-e '(+ 2 3))
                                       (parse-e '(+ 2 3))
                                       (parse-e '(+ 2 3)))))
(print (unparse-e (if-c-when-necessary (parse-e '(+ 2 3))
                                       (parse-e '(+ 2 4))
                                       (parse-e '(+ 2 3)))))

(print (lookup '+ (each parse-def '())))
(print (lookup '+ (each parse-def '((car (pair))
                                    (+ (m n))))))

(print (undefined? '+ (each parse-def '())))
(print (undefined? '+ (each parse-def '((car (pair))
                                        (+ (m n))))))

(print (bound? 'x '(a b c)))
(print (bound? 'x '(a x y)))
(print (bound? 'x 'any))

(let op-defs '((atom? (x))              ;XXX some different names
               (car (p))
               (cdr (p))
               (nat? (x))
               (size (x))
               (= (x y))
               (+ (m n))
               (< (m n))))

(print (expr? (each parse-def op-defs)
              'any
              (parse-e '(car (if (+ a 2) (cdr x) (cdr (car x)))))))

(print (call-arity? (each parse-def op-defs)
                    (parse-e '(+ 2 3 4))))

(print (steps? (each parse-def op-defs)
               (parse-steps '((XXX-path (+ a b))))))
(print (steps? (each parse-def op-defs)
               (parse-steps '((XXX-path (+ 1 2))))))

;; TODO check defs?
;; TODO check seed?
;; TODO check proof?

(print (unparse-e (sub-e '(a)
                         (each parse-e '((< 2 3)))
                         (parse-e '(+ a 3)))))

;(let eg1 (parse-def '(

(print (defs? prelude '()))

(print (bob/step prelude 
                 (parse-e '(car (cons 'ham '(eggs))))
                 (parse-steps '())))
(print (bob/step prelude 
                 (parse-e '(car (cons 'ham '(eggs))))
                 (parse-steps '(((1) (cons 'ham '(eggs)))
                                (() (car '(ham eggs)))))))

;; ch1/ex8
(print (bob/step prelude 
                 (parse-e '(car (cons (= (cons x y) (cons x y))
                                      '(and crumpets))))
                 (parse-steps '(((1 1) (equal-same (cons x y)))
                                ((1) (cons #yes '(and crumpets)))
                                (() (car '(#yes and crumpets)))))))
