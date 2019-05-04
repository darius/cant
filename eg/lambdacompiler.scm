;; Compile call-by-value lambda calculus to a machine with flat closures.

;; The machine has these registers:
;;   local - the argument to the current function
;;   closure - the closure pointer of the current function
;;   stack pointer - points to the top of the stack
;;   return-addr
;;   
;; Compiled code is a sequence of instructions:
;; XXX for 'k instructions' below, think of it as k slots in the list of instructions, in general.
;;   (LOCAL)      - push from the local register onto the stack
;;   (<number n>) - push the nth free variable of the closure
;;   (MAKE-CLOSURE m n <m instructions...> <n instructions>)
;;                - allocate a closure on the heap, holding m+1 values:
;;                  - a function pointer, pointing to the start of the <n instructions>
;;                  - m values, each one fetched in turn by executing the <m instructions>
;;                    each of which is a simple accessor instruction, i.e. either LOCAL
;;                    or a number.
;;   (RETURN)     - hold onto the top of the stack as the return value,
;;                  pop the former local, closure, and return address and 
;;                  push the return value back onto the stack,
;;                  and go to the return address.
;;   (PUSHCONT n) - ...
;;   (INVOKE)     - ...

;; XXX unfinished untested
(to (run-machine code argument)

  (to (running code local closure ret stack)
    (to (access insn)
      (match insn
        ('local local)
        ((? number?) (closure insn))))
    (match code
      (`(make-closure ,m ,n ,@rest)
       (let new-closure (array<-list (cons (rest .slice m)
                                           (each access (rest .slice 0 m)))))
       (running (rest .slice (+ m n)) local closure ret (cons new-closure stack)))
      (`(invoke ,@_)
       (let `(,argument ,callee ,@old-stack) stack)
       (running (callee 0) argument callee ret `(,local ,closure ,@old-stack)))
      (`(return ,@_)
       (let `(,return-value ,new-local ,new-closure ,new-ret ,@new-stack) stack)
       (running ret new-local new-closure new-ret (cons return-value new-stack)))
      (`(pushcont ,n ,@code1)
       (let new-ret (code1 .slice n))
       (let new-stack `(,local ,closure ,ret ,@stack))
       (running code1 local closure new-ret new-stack))
      (`(halt ,@_)                      ;XXX need to emit this somewhere
       stack.first)
      (`(,accessor ,@code1)    ;; ugh, defaulty representation
       (running code1 local closure ret (cons (access accessor) stack)))))

  (running code argument 'XXX 'XXX '()))

(to (compile lexp)
  ((parse lexp) .compile global-scope '(halt)))

(to (parse lexp)
  (match lexp
    ((? symbol?)           (var-ref<- lexp))
    (`(lambda (,v) ,body)  (abstraction<- v (parse body)))
    (`(,operator ,operand) (call<- (parse operator)
                                   (parse operand)))))

;; Variable reference
(to (var-ref<- v)
  (make ({.free-vars} (set<- v))
        ({.compile s k} (cons (s v) k))))

;; Lambda expression
(to (abstraction<- v body)
  (let free-vars (body.free-vars .difference (set<- v)))
  (make ({.free-vars} free-vars)
        ({.compile s k}
         (let code (body .compile (scope<- v free-vars.keys.inverse) '(return)))
         `(make-closure
           ,free-vars.count ,code.count ,@(each s free-vars.keys)
           ,@code ,@k))))

;; Application
(to (call<- operator operand)
  (make ({.free-vars} (operator.free-vars .union operand.free-vars))
        ({.compile s k}
         (let code (operand .compile s (operator .compile s '(invoke))))
         (if (= k '(return))
             code
             `(pushcont ,code.count ,@code ,@k)))))


;; Scopes (called 's' above)

(to (global-scope v)
  (error "Unbound variable" v))

(to ((scope<- param var-offsets) v)
  (if (= v param)
      'local
      (+ 1 (var-offsets v))))


;; Smoke test

(print (compile
;  '(lambda (x) x)
;  '(lambda (x) (x x))
;  '(lambda (x) y)
  '((lambda (x) (lambda (y) x)) (lambda (z) z))
;        'x
))
