(define (compile lexp)
  ((parse lexp) .compile global-static-env '(halt)))

(define (parse lexp)
  (match lexp
    ((: symbol?)        (var-ref<- lexp))
    (('lambda (v) body) (abstraction<- v (parse body)))
    ((operator operand) (call<- (parse operator)
                                (parse operand)))))

;; Variable reference
(define (var-ref<- v)
  (make ({.free-vars} (list<- v))
        ({.compile s k} (cons (s v) k))))

;; Lambda expression
(define (abstraction<- v body)
  (let free-vars (remove body.free-vars v))
  (make ({.free-vars} free-vars)
        ({.compile s k}
         (let code (body .compile (static-env<- v free-vars) '(return)))
         `(make-closure
           ,free-vars.count ,code.count ,@(each s free-vars)
           ,@code ,@k))))

;; Application
(define (call<- operator operand)
  (make ({.free-vars} (union operator.free-vars operand.free-vars))
        ({.compile s k}
         (let code (operand .compile s (operator .compile s '(invoke))))
         (if (= k '(return))
             code
             `(pushcont ,code.count ,@code ,@k)))))


;; Static environments (called 's' above)

(define (global-static-env v)
  (error "Unbound variable" v))

(define ((static-env<- param free-vars) v)
  (if (= v param)
      'local
      (+ 1 (free-vars .find v))))


;; Helpers

(define (union set1 set2)
  (for foldr ((x set1) (ys set2))
    (if (set2 .find? x) ys (cons x ys))))


;; Smoke test

(print (compile
;  '(lambda (x) x)
;  '(lambda (x) (x x))
;  '(lambda (x) y)
  '((lambda (x) (lambda (y) x)) (lambda (z) z))  ; XXX is output wrong?
;        'x
))
