;; Yet another bytecode compiler for yet another Lisp dialect
;; early work in progress

"
module: fndef*

fndef: (TO (name pattern*) . seq)

seq: (let|expr)+

let: (LET name expr)

expr: quoted | if | given | app | (MATCH expr (pattern . seq)*)
    | name | char | fixnum | boolean
    | module:name  ;; shorthand for (given (args...) (module:name args...))
    | {expr*}   ;; an immutable array, again

quoted: (QUOTE s-expr)
if:     (IF expr expr expr)
given:  (GIVEN (pattern*) . seq)
app:    (expr expr*)
"

;; Compiler

;; In the "assembly" language:
;; The label of an instruction is the count of instructions from itself to the end of code.
;; For example, in: A B {if-no 4} C {skip 2} D {return} E {return}
;;  the {if-no 4} branches to D; the {skip 2} jumps to E.

(to (compile-module module)
  (let scope {module-scope (map<- (for each ((`(to (,f ,@params) ,@_) module))
                                    `(,f {fndef ,f ,params.count})))})
  (for each ((def module))
    (compile-fndef scope def)))

(to (compile-fndef scope `(to (,name ,@params) ,@body))
  {fndef name params.count
         (compile-seq {scope scope params} ;XXX params are patterns, not just var names
                      body
                      '({return}))})

(to (compile-seq scope seq then)
  (match seq
    ('()
     (compile-exp scope #no then))
    (`((let ,pat ,exp) ,@rest)
     (compile-let scope pat exp rest then))
    (`(,exp)
     (compile-exp scope exp then))
    (`(,exp ,@rest)
     (compile-do scope exp rest then))))

(to (compile-let scope pat exp rest then)
  (error "stub: compile-let"))          ;XXX

(to (compile-do scope exp rest then)
  (compile-exp scope exp
               `({pop} ,@(compile-seq scope rest then))))

(to (compile-exp scope exp then)
;  (print `(compile-exp ,exp ,then))
  (match exp
    (`(,(? symbol? tag) ,@_)
     (compile-compound scope tag exp then))
    (`(,operator ,@operands)
     (compile-call scope operator operands then))
    ((? symbol?)
     (compile-var-ref scope exp then))
    ((? self-evaluating?)
     (compile-constant exp then))))

(to (compile-var-ref scope name then)
  (match scope
    ({module-scope _}
     (error "Unbound variable" name))   ;XXX
    ({scope parent frame}
     (match (frame .find name #no)
       (#no (error "Unbound variable" name)) ;XXX
       (n `({local ,n} ,@then))))))                     ;TODO generalize

(to (compile-compound scope tag exp then)
  (match tag
    ('quote
     (let `',value exp)
     (compile-constant value then))
    ('if
     (let `(if ,test ,yeah ,@(optional nope)) exp)
     (let n (compile-exp scope nope then))
     (let y (compile-exp scope yeah `(,(skip then) ,@n)))
     (compile-exp scope test `({if-no ,(target n)} ,@y)))
    ('match
     (error "TBD"))
    ('given
     (error "TBD"))
    (_ (compile-call scope exp.first exp.rest then))))

(to (compile-call scope operator operands then)
  (match (and (symbol? operator) (scope-lookup scope operator))
    (#no
     (error "Only known calls for now" operator)) ;XXX
    ((and known {primitive prim n-params opcode})
     (surely (= n-params operands.count)) ;XXX stub
     (compile-operands scope operands `(,known ,@then)))
    ((and known {fndef name n-params})
     (surely (= n-params operands.count)) ;XXX stub
     (match then
       (`({return} ,@then2)
        (compile-operands scope operands
                          `(,known ,@then2))) ;XXX needs tailcall adjustment
       (_
        `({frame ,then.count}
          ,@(compile-operands scope operands `(,known ,@then))))))))

(to (scope-lookup scope name)
  (match scope
    ({module-scope defs-map}
     (or (defs-map .get name)
         (match (primitives-2 .get name)
           ((? yeah? index) {primitive name 2 (+ index first-prim2-opcode)})
           (#no (error "Unbound variable" name)))))
    ({scope parent frame}
     (match (frame .find name #no)
       (#no (scope-lookup parent name))
       (n `({local ,n} ,@then))))))                    ;TODO generalize


(let primitives-2 ('#(= - + *) .inverse))
(let first-prim2-opcode 10)

(to (compile-operands scope operands then)
  (begin compiling ((operands operands)) ;of course this could currently be a foldr
    (if operands.empty?
        then
        (compile-exp scope operands.first
                     (compiling operands.rest)))))

(to (compile-constant value then)
  `({literal ,value} ,@then))

(to (return? then)
  (match then
    (`({return} ,@_) #yes)
    (_               #no)))

(to (skip code)
  (if (return? code) 
      code.first
      {skip (target code)}))

(to (target code)
  code.count)


;; Assembler

;; For the moment, assume all numeric values fit in one byte.
;; Let's try going with no frame pointer and see if it's a pain.
;; OTOH if we'll have a closure pointer then it doesn't feel so
;; excessive to save/restore a frame pointer as well.

(to (dump (and def {fndef name n-params assembly}))
  (format "def: ~d ~w\n" name n-params)
  (let n assembly.count)
  (for each! ((`(,i ,insn) assembly.items))
    (format "  ~2w ~w\n" (- n i) insn))
  (newline)
  (print (assemble `(,def)))
  (newline))

(to (assemble fndefs)
  (let buf (flexarray<-))
  (let constants (flexarray<-))
  (let constant-index (map<-))
  (let entry-map (map<-))
  (let labels (map<-))

  (to (idx<-constant value)
    (or (constant-index .get value)
        (do (let i constants.count) ;this is kinda wordy
            (constant-index .set! value i)
            (constants .push! value)
            i)))

  (to (emit opcode @args)
    (print `(emit ,opcode ,@args))
    (for each! ((arg (reverse args)))
      (buf .push! arg))
    (buf .push! opcode))

  (to (def-label label)
    (labels .set! label buf.count))

  (to (offset<-label label)
    (let target (labels label))
    (let offset (- buf.count target)) ;XXX right? wrong?
    offset)

  (to (emit-invoke name arity)
    (emit 5 (idx<-constant name) arity))

  (to (assemble-fndef {fndef entry-name n-params assembly})
    (for each! ((`(,i ,insn) ((reverse assembly) .items)))
      (print `(assembling ,insn))
      (match insn
        ({halt}                 (emit 0))
        ({return}               (emit 1))
        ({literal value}        (emit 2 (idx<-constant value)))
        ({skip label}           (emit 3 (offset<-label label)))
        ({if-no label}          (emit 4 (offset<-label label)))
        ({fndef name arity}     (emit-invoke name arity)) ;XXX check arity somewhere
        ({nip m n}              (emit 6 m n))
        ({frame label}          (emit 7 (offset<-label label)))
        ({local n}              (emit 8 n))
        ({pop}                  (emit 9))
        ({primitive _ _ opcode} (emit opcode))
        )
      (def-label (+ i 1)))
    (entry-map .set! entry-name buf.count))

  (each assemble-fndef fndefs)
  (emit 0)                              ;halt
  (print `(assembling init))
  (def-label 'halt-at)
  (print `(def halt-at))
  (emit-invoke (if (entry-map .maps? 'main)
                   'main
                   (do (let {fndef name _ _} fndefs.first)
                       name))
               0)
  (print `(invoke))
  (emit 7 (offset<-label 'halt-at))
  (print `(used halt-at))

  (let code (array<-list (reverse buf)))
  (let literals (array<-list constants))
  (let entry-points (for each ((`(,name ,rev-addr) entry-map.items))
                      `(,name ,(- buf.count rev-addr))))  ;TODO some sort of map.each method?
  {object-module code literals entry-points})


;; Virtual machine interpreter

(to (run {object-module code literals entry-points} fn-name)
  (let entry-table (map<- entry-points))  ;; TODO pre-link the entry-points (until a module reload)
  (let stack (array<-count 100))        ;XXX for the moment
  ;; The stack grows upwards; sp is the index of the top element.
  (begin running ((fp -1) (pc 0) (sp -1))       ;gonna need a closure-pointer too
    (print `(running ,fp ,pc ,sp : ,(code pc) <> ,(stack .slice 0 (+ sp 1))))
    (match (code pc)
      (0  ; halt
       (stack sp))
      (1  ; return
       (let result (stack sp))
       (let new-sp (- fp 2))
       (let new-fp (stack new-sp))
       (let new-pc (stack (+ new-sp 1)))
       (stack .set! new-sp result)
       (running new-fp new-pc new-sp))
      (2  ; literal n
       (let value (literals (code (+ pc 1))))
       (let new-sp (+ sp 1))
       (stack .set! new-sp value)
       (running fp (+ pc 2) new-sp))
      (3  ; skip n
       (let offset (code (+ pc 1)))
       (running fp (+ pc 2 offset) sp))
      (4  ; if-no n
       (let offset (code (+ pc 1)))
       (match (stack sp)
         (#no (running fp (+ pc 2 offset) (- sp 1)))
         (_   (running fp (+ pc 2)        (- sp 1)))))
      (5  ; invoke m n -- jump to the defn named by (literals m) with a frame of n args
       (let m (code (+ pc 1)))
       (let n (code (+ pc 2)))
       (let new-pc (entry-table (literals m)))
       (let new-fp (+ sp 1 (- n)))
       (running new-fp new-pc sp))
      (6  ; nip m n -- keep the top n stack elements; drop the next m. (preparing for a tailcall.)
       ;; XXX I guess we'll switch to CALL and TAILCALL ops
       (let m (code (+ pc 1)))
       (let n (code (+ pc 2)))
       ;; example: nip 3 2
       ;; indices 0 1 2 3 4 5 6    sp=6
       ;;         m n a b c y z
       ;;      -> m n y z          sp=3
       ;; stack[sp-n-m+1..sp-n) <-- stack[sp-n+1..sp+1)
       (let hi (+ sp 1))
       (stack .move! (- hi n m)
              stack (- hi n) hi)
       (running fp (+ pc 3) (- sp m)))
      (7  ; frame n -- push a return address
       (let offset (code (+ pc 1)))
       (stack .set! (+ sp 1) fp)              ; saved frame pointer
       (stack .set! (+ sp 2) (+ pc 2 offset)) ; return address
       (running fp (+ pc 2) (+ sp 2)))
      (8  ; local n
       (let n (code (+ pc 1)))
       (stack .set! (+ sp 1) (stack (+ fp n)))
       (running fp (+ pc 2) (+ sp 1)))
      (9  ; pop
       (running fp (+ pc 1) (- sp 1)))
      (10 ; primitive =
       (let result (= (stack (- sp 1)) (stack sp)))
       (stack .set! (- sp 1) result)
       (running fp (+ pc 1) (- sp 1)))
      (11 ; primitive -
       (let result (- (stack (- sp 1)) (stack sp)))
       (stack .set! (- sp 1) result)
       (running fp (+ pc 1) (- sp 1)))
      (12 ; primitive +
       (let result (+ (stack (- sp 1)) (stack sp)))
       (stack .set! (- sp 1) result)
       (running fp (+ pc 1) (- sp 1)))
      (13 ; primitive *
       (let result (* (stack (- sp 1)) (stack sp)))
       (stack .set! (- sp 1) result)
       (running fp (+ pc 1) (- sp 1)))
      )))


;; Smoke test

;(print (compile-fndef '(to (foo x) (if 'a 'b 'c))))
;(print (compile-exp {global-scope} '(if 'a 'b 'c) '({return})))
;(print (compile-fndef '(to (f x) x)))

;(to (run x y) x)

(let eg (compile-module '((to (g) 'x))))
(dump eg.first)
(print (run (assemble eg) 'g))
(print '---)

(let eg2 (compile-module '((to (f) (if (= 1 2) 'yes 'no)))))
(print (run (assemble eg2) 'f))
(print '---)

;; (dump (compile-fndef eg-scope '(to (g x) (if (= x x) 'yes 'no))))
;; (dump (compile-fndef eg-scope '(to (h x y) (if (= x y) 'yes (h (+ x 1) (- y 1))))))

;; (let eg (compile-fndef eg-scope '(to (g) 'x)))
;; (dump eg)
;; (print (run (assemble eg)))

(when #yes
(let eg3 (compile-module '((to (main) (f 10))
                           (to (f n) (if (= n 1) 1 (* n (f (- n 1))))))))
(print (run (assemble eg3) 'main))
)
