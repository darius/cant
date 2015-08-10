(load "stdlib.scm")
;(load "traceback.scm")

(define (compile lexp)
  (.compile (parse lexp) global-static-env '(halt)))

(define (parse lexp)
  (cond ((symbol? lexp)
         (var-ref<- lexp))
        ((is? (lexp 0) 'lambda)
         (abstraction<- ((lexp 1) 0)
                        (parse (lexp 2))))
        (else
         (call<- (parse (lexp 0))
                 (parse (lexp 1))))))

;; Variable reference
(define (var-ref<- v)
  (make (.free-vars () (list<- v))
        (.compile (s k) (cons (s v) k))))

;; Lambda expression
(define (abstraction<- v body)
  (let ((free-vars (delq v (.free-vars body))))
    (make (.free-vars () free-vars)
          (.compile (s k)
            (let ((code (.compile body (static-env<- v free-vars) '(return))))
              (chain (list<- 'make-closure (.count free-vars) (.count code))
                     (map s free-vars)
                     code
                     k))))))

;; Application
(define (call<- operator operand)
  (make (.free-vars () (union (.free-vars operator) (.free-vars operand)))
        (.compile (s k)
          (let ((code (.compile operand s (.compile operator s '(invoke)))))
            (if (is? (.first k) 'return)
                code
                (chain (list<- 'pushcont (.count code)) code k))))))


;; Static environments (called 's' above)

(define (global-static-env v)
  (error "Unbound variable" v))

(define (static-env<- param free-vars)
  (given (v)
    (if (is? v param)
        'local
        (+ 1 (list-index free-vars v)))))


;; Smoke test

(print (compile
;  '(lambda (x) x)
;  '(lambda (x) y)
  '((lambda (x) (lambda (y) x)) (lambda (z) z))  ; XXX is output wrong?
))