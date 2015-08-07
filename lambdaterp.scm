;; Let's work out a source-level debugger in a simpler setting,
;; the call-by-value lambda calculus.
;; (That's the goal; not there yet.)

(load "stdlib.scm")
(load "traceback.scm")

;; Conventions:
;;  c constant value
;;  v variable name (a symbol)
;;  r environment
;;  k continuation
;;  others an AST or a value

(define (interpret lexp)
  ('evaluate (parse lexp) global-env halt))

(define (parse lexp)
  (if (symbol? lexp)
      (var-ref<- lexp)
      (if (number? lexp)
          (constant<- lexp)
          (if (is? (lexp 0) 'lambda)
              (abstraction<- ((lexp 1) 0)
                             (parse (lexp 2)))
              (call<- (parse (lexp 0))
                      (parse (lexp 1)))))))


;; ASTs and continuations

(define halt
  (make ('take (val) val)))

;; Constant
(define (constant<- c)
  (make ('evaluate (r k) ('take k c))))

;; Variable reference
(define (var-ref<- v)
  (make ('evaluate (r k) (lookup r v k))))

;; Lambda expression
(define (abstraction<- v body)
  (make ('evaluate (r k)
          ('take k (make
                     ('call (arg k2)
                       ('evaluate body (extend r v arg) k2)))))))

;; Application
(define (call<- operator operand)
  (make ('evaluate (r k)
          ('evaluate operator r
            (make ('take (fn)
                    ('evaluate operand r
                      (make ('take (arg)
                              ('call fn arg k))))))))))


;; Built-ins

(define prim+
  (make ('call (arg1 k1)
          ('take k1 (make ('call (arg2 k2)
                            ('take k2 ('+ arg1 arg2))))))))


;; Environments

(define global-env
  (list<- (list<- '+ prim+)))

(define (extend r v val)
  (cons (list<- v val) r))

(define (lookup r v k)
  (let ((record (assq v r)))
    (if record
        ('take k (record 1))
        (error "Unbound var in interpret" v))))


;; Smoke test

(print (interpret
  '((lambda (x) ((+ x) 2)) 1)
;  '(lambda (x) y)
;  '((lambda (x) (lambda (y) x)) (lambda (z) z))  ; XXX is output wrong?
))