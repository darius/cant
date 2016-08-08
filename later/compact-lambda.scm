(define (compile lexp)
  ((parse lexp) .compile '(HALT)))

(make parse
  (((: v symbol?))      (var-ref<- v))
  ((('lambda (v) body)) (abstraction<- v (parse body)))
  (((operator operand)) (call<- (parse operator)
                                (parse operand))))

;; Variable reference
(define (var-ref<- v)
  (given {.compile k}
    `(VAR ,v ,@k)))

;; Lambda expression
(define (abstraction<- v body)
  (given {.compile k}
    (let code (body .compile '(RET)))
    `(LAM ,v ,code.count ,@code ,@k)))

;; Application
(define (call<- operator operand)
  (given {.compile k}
    (let code (operator .compile (operand .compile '(CALL))))
    (match k
      ('(RET) code)
      (_      `(SAVE ,code.count ,@code ,@k)))))


;; Smoke test

(print (compile
;  '(lambda (x) x)
;  '(lambda (x) y)
  '((lambda (x) (lambda (y) x)) (lambda (z) z))
))
