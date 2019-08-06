;; a la The Little Prover
;; #yes/#no for 't/'nil
;; Renamed some operators: atom? nat? =
;;   (we also allow the book's "atom" and "natp", but it'd be a bit of a pain to support
;;   both "equal" and "=")
;; I'm adding an assumption that arguments are well-formed or well-typed. (to be fleshed out)
;; Going to assume things are pre-parsed into structs
;;   (I'm not sure that's a net win for readability)
;; Factored def structs together, moved rators together with axioms

;; More notes & nits about Appendix C:
;; * They have a forward ref to arity? contradicting their claim to bottom-up style
;; * Mis-grouping of code and literate-doc blocks
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
;;   We'd have to handle local variables (not just parameters). Would it be worth it?
;; * Might be nice to have a kind of Pre-Scheme-ish thing that expands out higher-order functions
;;   and I guess mutual recursion. You'd want it to instantiate theorems at the same time.
;; * I don't like 'E' as the tag for an if's else-branch, because
;;   there's also 'e' as the abbrev for an expression.

(import (use 'pretty-print) pp)    ;XXX for debugging


;; Types:

;; proof:
;;   {proof def seed steps}
;; where seed is an e or #no            XXX make that {constant #no}?

;; def:
;;   {def name formals meaning}
;; meaning:
;;   {fun e}
;;   {thm e}
;;   {op fn}    "rator?" in J-Bob

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
  (let result (fn @args))
  (when loud? (pp `(<<< ,fn : ,result)))
  result)

(let loud? #no)


;; TODO explain me
;; TODO did I refactor duplication correctly?
;;   Seems unlikely since it got so much shorter.

(to (rewrite/define+ defs proofs)
  (be proofs
    ('() defs)
    (`({proof ,def ,seed ,steps} ,@rest-proofs)
     (if (= yes-c (rewrite/prove defs def seed steps))
         (rewrite/define+ (append defs def) rest-proofs)
         defs))))


;; TODO explain me

(to (rewrite/prove+ defs proofs)
  (be proofs
    ('() yes-c)
    (`({proof ,def ,seed ,steps} ,@rest-proofs)
     (let e (rewrite/prove+ (append defs def) rest-proofs))
     (if (= e yes-c)
         (rewrite/prove defs def seed steps) ;y'know, if we did this first it'd be more like rewrite/define+
         e))))

(to (rewrite/prove defs def seed steps)
  (let {def _ _ meaning} def)
  (be meaning
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

;; TODO incohesion in that we're assuming the def corresponds to the app
(to (equality/def claim path app {def name formals meaning})
  (when loud?
    (pp `(equality/def ,(unparse-e claim) ,path ,(unparse-e app) ,(unparse-def {def name formals meaning}))))
  (let {call _ args} app)
  (let result
    (equality/path claim path
                 (be meaning
                   ;; TODO do these constructed (= foo bar) exprs ever cause rewriting-in-reverse?
                   ;;   Or is this just a fancy way of substituting (eval-op app), etc.?
                   ({op fn}    `{call = (,app ,(eval-op fn args))})
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
  (be concl-inst
    ({call '= `(,arg1 ,arg2)} (equality focus arg1 arg2))
    (_                        focus)))

;; Taking "a=b" as a rewrite rule, rewrite focus if possible.
(to (equality focus a b)
  (hm (if (= focus a) b)
      (if (= focus b) a)
      (else           focus)))


;; "Check the premises against the instantiated conclusion." (?)
;; As far as possible, remove any top-level if-then-elses from thm.
;; It's possible when the if-question appears also as the question of
;; some 'if' in path through e1; then dig out the branch of thm's 'if'
;; corresponding to the next branch of the path through e1's 'if'.
;; TODO check that this makes sense on some examples
(to (follow-prems path e1 thm)          ;TODO rename e1?
  (be thm
    ({if q a e}
     (hm (if (prem-match? 'A q path e1) (follow-prems path e1 a))
         (if (prem-match? 'E q path e1) (follow-prems path e1 e))
         (else                          thm)))
    (_ thm)))

;; Somewhere along path through e, does some if have a question equal to
;; prem, followed by dir as the next step in the path?
;; TODO more efficient to check that the next step is either 'A or 'E
;;      and return that, or #no. Then we wouldn't have to call this twice.
(to (prem-match? dir prem path e)
  (begin matching ((path path) (e e))
    (and (not path.empty?)
         (or (and (= path.first dir)
                  (be e
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
  (be dir
    ('Q       (be e ({if q _ _}    q)))
    ('A       (be e ({if _ a _}    a)))
    ('E       (be e ({if _ _ e}    e)))
    ((? nat?) (be e ({call _ args} (get-arg dir args))))))

(to (set-at-direction dir e1 e2)
  (be dir
    ('Q       (be e1 ({if q a e}    {if e2 a e})))
    ('A       (be e1 ({if q a e}    {if q e2 e})))
    ('E       (be e1 ({if q a e}    {if q a e2})))
    ((? nat?) (be e1 ({call f args} {call f (set-arg dir args e2)})))))

(to (get-arg n args)
  (args (- n 1)))

(to (set-arg n args e)
  (if (= n 1)
      (link e args.rest)
      (link args.first (set-arg (- n 1) args.rest e))))


;; Check okayness of a path.

(to (focus-is-at-path? path e)
  (or path.empty?
      (and (focus-is-at-direction? path.first e)
           (focus-is-at-path? path.rest
                              (get-at-direction path.first e)))))
  
(to (focus-is-at-direction? dir e)
  (be dir
    ('Q       (be e ({if _ _ _}    #yes) (_ #no)))
    ('A       (be e ({if _ _ _}    #yes) (_ #no)))
    ('E       (be e ({if _ _ _}    #yes) (_ #no)))
    ((? nat?) (be e ({call _ args} (<= 1 dir args.count))
                       (_ #no)))))


;; Build a totality claim.

(to (totality/claim meas {def name formals {fun body}})
  (if (not meas)                        ;XXX might need to unparse a constant
      {constant ((calls-to name body) .empty?)}
      {if {call 'nat? `(,meas)}
          (totality/if meas name formals body)
          no-c}))

(to (totality/if meas f formals body)
  (be body
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
  (be seed
    (#no           body)
    ({call f args} (induction/defun args body (lookup f defs)))))

(to (induction/defun vars claim {def f formals {fun body}})
  (induction/if vars claim f (sub-e formals vars body)))

(to (induction/if vars claim f body)
  (be body
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
  (be e
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
  (be e
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
  (hm (if vars.empty?         {variable name})
      (if (= vars.first name) args.first)
      (else                   (sub-var vars.rest args.rest name))))


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
  (be meaning
    ({fun _} (expr? defs formals seed))
    ({thm _} (induction-scheme? defs formals seed))
    (_       #no)))          ;does this come up?

(to (induction-scheme? defs vars e)
  (be e
    ({call f args} (induction-scheme-for? (lookup f defs) vars e))
    (_             #no)))

(to (induction-scheme-for? def vars {call _ args})
  (be def
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
  (be meaning
    ({thm body} (and (undefined? name known-defs)
                     (expr? known-defs formals body))) ;TODO y'know, this could use extend-rec too
    ({fun body} (and (undefined? name known-defs)
                     (expr? (extend-rec known-defs def) formals body)))
    ({op _}     #yes)))               ;XXX different from the book.
;;    (_          #no)))                ;TODO needed? or should be #yes?

(to (extend-rec defs {def name formals meaning})
  (be meaning
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
         (be meaning
           ({op _} (every constant? args))
           (_      (exprs? defs 'any args)))))) ;XXX so where do we check the boundness of args?


;; Check okayness of expressions.
;; Variables must be bound according to vars;
;; calls must name a defun or operator with the correct arity.

(to (expr? defs vars e)
  (be e
    ({constant _}    #yes)
    ({variable name} (bound? name vars))
    ({if q a e}      (exprs? defs vars `(,q ,a ,e)))
    ({call _ es}     (and (call-arity? defs e)
                          (exprs? defs vars es)))))

(to (exprs? defs vars es)
  (for every ((e es))
    (expr? defs vars e)))

(to (call-arity? defs {call name args})
  (be (lookup name defs)
    ({def _ formals {op _}}  (arity? formals args))
    ({def _ formals {fun _}} (arity? formals args))
    (_                       #no))) ;TODO maybe assume this case never happens. Then just ignore the meaning field.

(to (arity? formals args)
  (= formals.count args.count))


;; Name lookup.

(to (lookup name defs)
  (be defs
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
  (be e
    ({constant _} #yes)
    (_            #no)))

(to (variable? e)
  (be e
    ({variable _} #yes)
    (_            #no)))

(to (if-c-when-necessary q a e)
  (if (= a e)
      a
      {if q a e}))

(to (conjoin es)
  (be es
    ('()         yes-c)
    (`(,e)       e)
    (`(,e ,@es1) {if e (conjoin es1) no-c})))

(to (implies es e)
  (be es
    ('()          e)
    (`(,e1 ,@es1) {if e1 (implies es1 e) yes-c})))


;; Apply an operator to values.

(to (eval-op fn args)
  {constant (fn @(for each (({constant value} args))
                   value))})


;; Runtime.

(to (atom? x)   (not (link? x)))                      ;XXX right?
(to (bob-car x) (and (link? x) x.first))
(to (bob-cdr x) (and (link? x) x.rest))
(to (nat? x)    (count? x))
(to (bob+ x y)  (and (number? x) (number? y) (+ x y)))
(to (bob< x y)  (and (number? x) (number? y) (< x y)))

(to (size x)
  (be x
    (`(,h ,@t) (+ (size h) (size t)))
    (_ 1)))

(let x-ops
  `(
    (atom? (x)  ,atom?)
    (atom (x)   ,atom?)
    (car (x)    ,bob-car)
    (cdr (x)    ,bob-cdr)
    (nat? (x)   ,nat?)
    (natp (x)   ,nat?)
    (size (x)   ,size)
    (= (x y)    ,=)
    (cons (x y) ,link)
    (+ (x y)    ,bob+)
    (< (x y)    ,bob<)
    ))


;; Unparse.

(to (unparse-def {def name formals meaning})
  (be meaning
    ({fun body} `(defun ,name ,formals ,(unparse-e body)))
    ({op fn}    `(defop ,name))
    ({thm body} `(dethm ,name ,formals ,(unparse-e body)))))

(to (unparse-e e)
  (be e
    ({constant datum} `',datum)
    ({variable name}  name)
    ({if q a e}       `(if ,(unparse-e q)
                           ,(unparse-e a)
                           ,(unparse-e e)))
    ({call name es}   `(,name ,@(each unparse-e es)))))


;; Parse.

(to (parse-proof xproof)
  (be xproof
    (`(,xdef ,xseed ,@xsteps)
     {proof (parse-def xdef)
            (and xseed (parse-e xseed))
            (parse-steps xsteps)})))

(to (parse-steps xsteps)
  (surely (list? xsteps) "Steps syntax")
  (each parse-step xsteps))

(to (parse-step xstep)
  (be xstep
    (`(,xpath ,xe)
     ;; TODO check path well-formed
     ;; TODO check that xe is a call? I think?
     {step xpath (parse-e xe)})))

(to (parse-def xdef)
  (be xdef
    (`(defun ,(? symbol? name) ,(? formals? formals) ,body)
     {def name formals {fun (parse-e body)}})
    (`(dethm ,(? symbol? name) ,(? formals? formals) ,body)
     {def name formals {thm (parse-e body)}})
    (`(,(? symbol? name) ,(? formals? formals) ,fn)
     {def name formals {op fn}})))

(to (formals? x)
  (and (list? x)
       (every symbol? x)
       (set? x)))

(to (parse-e xe)
  (be xe
    ((? number?)     {constant xe})
    ((? claim?)      {constant xe})
    (''nil           no-c)
    (''t             yes-c)
    (`',datum        {constant datum})
    ((? symbol?)     {variable xe})
    (`(if ,x1 ,x2 ,x3)         {if (parse-e x1) (parse-e x2) (parse-e x3)})
    (`(,(? symbol? f) ,@xargs) {call f (each parse-e xargs)})))


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
  (= xs.count xs.range.count))


;; Prelude

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
  (each parse-def (chain x-ops x-axioms)))


;; Smoke test

(print (set? '(a b c)))
(print (set? '(a b c b)))

(print (subset? '(a b c) '(c a)))
(print (subset? '(c a) '(a b c)))

(print (append '(a b c) 'a))
(print (append '(a b c) 'd))

(print (list-union '(a b c) '(c d b e)))

(print (unparse-e (if-c-when-necessary (parse-e '(+ 2 3))
                                       (parse-e '(+ 2 3))
                                       (parse-e '(+ 2 3)))))
(print (unparse-e (if-c-when-necessary (parse-e '(+ 2 3))
                                       (parse-e '(+ 2 4))
                                       (parse-e '(+ 2 3)))))

(print (bound? 'x '(a b c)))
(print (bound? 'x '(a x y)))
(print (bound? 'x 'any))

(print (expr? prelude 'any
              (parse-e '(car (if (+ a 2) (cdr x) (cdr (car x)))))))

(print (call-arity? prelude
                    (parse-e '(+ 2 3 4))))

(print (steps? prelude
               (parse-steps '((XXX-path (+ a b))))))
(print (steps? prelude
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
                                ((1) (cons 't '(and crumpets)))
                                (() (car '(#yes and crumpets)))))))

(to (J-Bob/define defs x-proofs)
  (bob/define defs
              (each parse-proof x-proofs)))

(let defun-pair
  (J-Bob/define prelude
                '(((defun pair (x y)
                     (cons x (cons y '())))
                   #no))))
;(print `(defun-pair ,defun-pair))

(let defun-first-of
  (J-Bob/define defun-pair
                '(((defun first-of (x)
                     (car x))
                   #no))))
;(print defun-first-of)

(let defun-second-of
  (J-Bob/define defun-first-of
                '(((defun second-of (x)
                     (car (cdr x)))
                   #no))))
;(print defun-second-of)

(let dethm-first-of-pair
  (J-Bob/define defun-second-of
                '(((dethm first-of-pair (a b)
                          (= (first-of (pair a b)) a))
                   #no
                   ((1 1) (pair a b))
                   ((1) (first-of (cons a (cons b '()))))
                   ((1) (car/cons a (cons b '())))
                   (() (equal-same a))))))
;(pp (each unparse-def dethm-first-of-pair))

(let dethm-second-of-pair
  (J-Bob/define dethm-first-of-pair
                '(((dethm second-of-pair (a b)
                          (= (second-of (pair a b)) b))
                   #no
                   ((1) (second-of (pair a b)))
                   ((1 1 1) (pair a b))
                   ((1 1) (cdr/cons a (cons b '())))
                   ((1) (car/cons b '()))
                   (() (equal-same b))))))
;(pp (each unparse-def dethm-second-of-pair))

(let defun-in-pair?
  (J-Bob/define dethm-second-of-pair
                '(((defun in-pair? (xs)
                     (if (= (first-of xs) '?) 't (= (second-of xs) '?)))
                   #no))))

(let dethm-in-first-of-pair
  (J-Bob/define defun-in-pair?
                '(((dethm in-first-of-pair (b)
                          (= (in-pair? (pair '? b)) 't))
                   #no
                   ((1 1) (pair '? b))
                   ((1) (in-pair? (cons '? (cons b '()))))
                   ((1 Q 1) (first-of (cons '? (cons b '()))))
                   ((1 Q 1) (car/cons '? (cons b '())))
                   ((1 Q) (equal-same '?))
                   ((1) (if-true 't (= (second-of (cons '? (cons b '()))) '?)))
                   (() (equal-same 't))))))

(let dethm-in-second-of-pair
  (J-Bob/define dethm-in-first-of-pair
                '(((dethm in-second-of-pair (a)
                          (= (in-pair? (pair a '?)) 't))
                   #no
                   ((1 1) (pair a '?))
                   ((1) (in-pair? (cons a (cons '? '()))))
                   ((1 Q 1) (first-of (cons a (cons '? '()))))
                   ((1 Q 1) (car/cons a (cons '? '())))
                   ((1 E 1) (second-of (cons a (cons '? '()))))
                   ((1 E 1 1) (cdr/cons a (cons '? '())))
                   ((1 E 1) (car/cons '? '()))
                   ((1 E) (equal-same '?))
                   ((1) (if-same (= a '?) 't))
                   (() (equal-same 't))))))
(pp (each unparse-def dethm-in-second-of-pair))
