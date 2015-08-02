(load "stdlib.scm")

(define (compile lexp)
  ('compile (expand lexp) global-static-env '(halt)))

(define (expand lexp)
  (if (symbol? lexp)
      (make-var lexp)
      (if (eq? ('car lexp) 'lambda)
          (make-lam ('car ('car ('cdr lexp)))
                    (expand ('car ('cdr ('cdr lexp)))))
          (make-app (expand ('car lexp))
                    (expand ('car ('cdr lexp)))))))

(define (symbol? x)
  (eq? ('type x) 'symbol))

(define (make-var v)
  (make ('free-vars () (list1 v))
        ('compile (r k) (cons (r v) k))))

(define (make-lam v e)
  (let ((free-vars (delq v ('free-vars e))))
    (make ('free-vars () free-vars)
          ('compile (r k)
            (let ((code ('compile e (make-static-env v free-vars) '(return))))
              (cons 'make-closure 
                    (cons (length free-vars)
                          (cons (length code)
                                (append3 (map r free-vars) code k)))))))))

(define (make-app e1 e2)
  (make ('free-vars () (union ('free-vars e1) ('free-vars e2)))
        ('compile (r k)
          (let ((code ('compile e2 r ('compile e1 r '(invoke)))))
            (if (eq? ('car k) 'return)
                code
                (cons 'pushcont (cons (length code) (append2 code k))))))))

(define (global-static-env v)
  (error "Unbound variable" v))

(define (make-static-env v free-vars)
  (lambda (v1)
    (if (eq? v1 v)
        'local
        ('+ 1 (list-index v1 free-vars)))))
