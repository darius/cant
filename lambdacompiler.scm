(load "stdlib.scm")
;(load "traceback.scm")

(define (compile lexp)
  ('compile (expand lexp) global-static-env '(halt)))

(define (expand lexp)
  (if (symbol? lexp)
      (var<- lexp)
      (if (is? (lexp 0) 'lambda)
          (lam<- ((lexp 1) 0)
                 (expand (lexp 2)))
          (app<- (expand (lexp 0))
                 (expand (lexp 1))))))

(define (var<- v)
  (make ('free-vars () (list<- v))
        ('compile (r k) (cons (r v) k))))

(define (lam<- v e)
  (let ((free-vars (delq v ('free-vars e))))
    (make ('free-vars () free-vars)
          ('compile (r k)
            (let ((code ('compile e (static-env<- v free-vars) '(return))))
              (chain (list<- 'make-closure ('count free-vars) ('count code))
                     (map r free-vars)
                     code
                     k))))))

(define (app<- e1 e2)
  (make ('free-vars () (union ('free-vars e1) ('free-vars e2)))
        ('compile (r k)
          (let ((code ('compile e2 r ('compile e1 r '(invoke)))))
            (if (is? ('first k) 'return)
                code
                (chain (list<- 'pushcont ('count code)) code k))))))

(define (global-static-env v)
  (error "Unbound variable" v))

(define (static-env<- v free-vars)
  (lambda (v1)
    (if (is? v1 v)
        'local
        ('+ 1 (list-index v1 free-vars)))))


;; Smoke test

(print (compile
;  '(lambda (x) x)
;  '(lambda (x) y)
  '((lambda (x) (lambda (y) x)) (lambda (z) z))  ; XXX is output wrong?
))