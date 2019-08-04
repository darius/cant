;; 'Compile' cbv lambda calculus without changing much.

(to (compile lexp)
  ((parse lexp) .compile '(HALT)))

(to (parse lexp)
  (be lexp
    ((? symbol?)           (var-ref<- lexp))
    (`(lambda (,v) ,body)  (abstraction<- v (parse body)))
    (`(,operator ,operand) (call<- (parse operator)
                                   (parse operand)))))

;; Variable reference
(to ((var-ref<- v) .compile k)
  `(VAR ,v ,@k))

;; Lambda expression
(to ((abstraction<- v body) .compile k)
  (let code (body .compile '(RET)))
  `(LAM ,v ,code.count ,@code ,@k))

;; Application
(to ((call<- operator operand) .compile k)
  (let code (operator .compile (operand .compile '(CALL))))
  (be k
    ('(RET) code)
    (_      `(SAVE ,code.count ,@code ,@k))))


;; Smoke test

(print (compile
;  '(lambda (x) x)
;  '(lambda (x) y)
  '((lambda (x) (lambda (y) x)) (lambda (z) z))
))
