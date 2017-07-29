;; Compile call-by-value lambda calculus to a machine with flat closures.

(to (compile lexp)
  ((parse lexp) .compile global-static-env '(halt)))

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
         (let code (body .compile (static-env<- v free-vars.keys) '(return)))
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


;; Static environments (called 's' above)

(to (global-static-env v)
  (error "Unbound variable" v))

(to ((static-env<- param free-vars) v)
  (if (= v param)
      'local
      (+ 1 (free-vars .find v))))


;; Smoke test

(print (compile
;  '(lambda (x) x)
;  '(lambda (x) (x x))
;  '(lambda (x) y)
  '((lambda (x) (lambda (y) x)) (lambda (z) z))  ; XXX is output wrong?
;        'x
))
