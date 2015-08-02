(load "stdlib.scm")

(define (compile lexp)
  ('compile (expand lexp) global-static-env '(halt)))

(define (expand lexp)
  (if (symbol? lexp)
      (var<- lexp)
      (if (is? ('first lexp) 'lambda)
          (lam<- ('first ('first ('rest lexp)))
                 (expand ('first ('rest ('rest lexp)))))
          (app<- (expand ('first lexp))
                 (expand ('first ('rest lexp)))))))

(define (symbol? x)
  (is? ('type x) 'symbol))

(define (var<- v)
  (make ('free-vars () (list<- v))
        ('compile (r k) (cons (r v) k))))

(define (lam<- v e)
  (let ((free-vars (delq v ('free-vars e))))
    (make ('free-vars () free-vars)
          ('compile (r k)
            (let ((code ('compile e (make-static-env v free-vars) '(return))))
              (cons 'make-closure 
                    (cons (length free-vars)
                          (cons (length code)
                                (chain (map r free-vars) code k)))))))))

(define (app<- e1 e2)
  (make ('free-vars () (union ('free-vars e1) ('free-vars e2)))
        ('compile (r k)
          (let ((code ('compile e2 r ('compile e1 r '(invoke)))))
            (if (is? ('first k) 'return)
                code
                (cons 'pushcont (cons (length code) (chain code k))))))))

(define (global-static-env v)
  (error "Unbound variable" v))

(define (make-static-env v free-vars)
  (lambda (v1)
    (if (is? v1 v)
        'local
        ('+ 1 (list-index v1 free-vars)))))


;; Smoke test

(print (compile
;  '(lambda (x) x)
  '((lambda (x) (lambda (y) x)) (lambda (z) z))  ; XXX is output wrong?
))