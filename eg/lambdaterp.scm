;; Let's work out a source-level debugger in a simpler setting,
;; the call-by-value lambda calculus.
;; (That's the goal; not there yet.)

;; Conventions:
;;  lexp    source form of lambda-calculus expression
;;  c       constant value
;;  v       variable name (a symbol)
;;  r       environment
;;  k       continuation
;;  others  an AST or a value

(define (parse lexp)
  (match lexp
    ((: symbol?)
     (var-ref<- lexp))
    ((: number?)
     (constant<- lexp))
    (('lambda (v) body)
     (abstraction<- v (parse body)))
    ((operator operand)
     (call<- (parse operator)
             (parse operand)))))

(define (interpret lexp)
  ((parse lexp) .evaluate global-env halt))


;; ASTs and continuations

(make halt
  ({.empty?} #yes)
  ({.inject k<-} halt)
  ({.take-step val} val)
  ({.take val} val))


;; Constant
(define (constant<- c)
  (make constant
    ({.source} c)
    ({.eval-step r k} (debugging (value-step<- constant r k)))
    ({.evaluate r k} (k .take c))))

;; Variable reference
(define (var-ref<- v)
  (make var-ref
    ({.source} v)
    ({.eval-step r k} (debugging (value-step<- var-ref r k)))
    ({.evaluate r k} (lookup r v k))))

;; Lambda expression
(define (abstraction<- v body)
  (make abstraction
    ({.source} `(& ,v ,body.source))
    ({.eval-step r k} (debugging (value-step<- abstraction r k)))
    ({.evaluate r k}
     (k .take (make
                ({.survey} `(,v -> <body>))
                ({.call arg k2}
                 (body .evaluate (extend r v arg) k2))
                ({.call-step arg k2}
                 (body .eval-step (extend r v arg) k2)))))))

;; Application
(define (call<- operator operand)
  (make app
    ({.source} `(,operator.source ,operand.source))
    ({.eval-step r k}
     (debugging (subeval-step<- operator r (ev-arg-cont<- operand r k))))
    ({.evaluate r k}
     (operator .evaluate r (ev-arg-cont<- operand r k)))))

(define (ev-arg-cont<- operand r k)
  (make ({.empty?} #no)
        ({.rest} k)
        ({.first} `(^ ,operand.source))
        ({.inject k<-} (ev-arg-cont<- operand r (k<- k)))
        ({.take fn}
         (operand .evaluate r (call-cont<- fn k)))
        ({.take-step fn}
         (operand .eval-step r (call-cont<- fn k)))
        ))

(define (call-cont<- fn k)
  (make ({.empty?} #no)
        ({.rest} k)
        ({.first} `(,(survey fn) ^))
        ({.inject k<-} (call-cont<- fn (k<- k)))
        ({.take arg}
         (fn .call arg k))
        ({.take-step arg}
         (fn .call-step arg k))
        ))


;; Built-in values

(define (survey value)
  (if (or (number? value) (symbol? value))
      value
      value.survey))

(make prim+
  ({.survey} '+)
  ({.call-step arg1 k1}
   XXX)
  ({.call arg1 k1}
   (if (number? arg1)
       (k1 .take (make
                   ({.survey} `(+ ,(survey arg1)))
                   ({.call-step arg2 k2}
                    XXX)
                   ({.call arg2 k2}
                    (if (number? arg2)
                        (k2 .take (+ arg1 arg2))
                        ;; XXX should supply self, too:
                        (debug k2 "Bad arg2 to +" (survey arg2))))))
       (debug k1 "Bad arg1 to +" (survey arg1)))))


;; Environments

(let global-env
  `((+ ,prim+)))

(define (extend r v val)
  `((,v ,val) ,@r))

(define (lookup r v k)
  (case ((assq v r) => (given (record) (k .take (record 1))))
        (else (debug k "Unbound var" v))))


;; Debugger
;; Instead of interacting at a prompt, it takes a list of commands,
;; for now, for ease of rerunning during development.

(let command-queue (box<- '()))

(define (next-command)
  (display "debug> ")
  (match command-queue.^
    (()
     (newline)
     #no)
    ((first @rest)
     (print first)
     (command-queue .^= rest)
     first)))

(define (debug k plaint irritant)
  (complain plaint irritant)
  (traceback k)
  (debugging (out-step<- k 'default-error-value)))

(define (complain plaint irritant)
  (display "Lambdaterp error: ")
  (write plaint)
  (display ": ")
  (write irritant)
  (newline))

(define (traceback k)
  (each! print k))

(define (debugging state)
  (let cmd (next-command))
  (if cmd (call state cmd) #no))

(define (value-step<- e r k)
  (make value-step
    ({.show}
     (display "ev-> ") (print e.source))
    ({.b}
     (traceback k)
     (debugging value-step))
    ({.continue}
     (e .evaluate r k))
    ({.hop}
     (e .evaluate r (k .inject debugger-trap-cont<-)))
    ({.step}
     value-step.hop)
    ))

(define (subeval-step<- e r k)
  (make subeval-step
    ({.show}
     (display "ev-> ") (print e.source))
    ({.b}
     (traceback k)
     (debugging subeval-step))
    ({.continue}
     (e .evaluate r k))
    ({.hop}
     (e .evaluate r (k .inject debugger-trap-cont<-)))
    ({.step}
     (e .eval-step r k))
    ))

(define (out-step<- k value)
  (make out-step
    ({.show}
     (display "<-ret ") (print (survey value)))
    ({.b}
     (traceback k)
     (debugging out-step))
    ({.continue}
     (k .take value))
    ({.hop}
     ((k .inject debugger-trap-cont<-) .take value))
    ({.step} 
     (k .take-step value))
    ({.value new-value}
     (debugging (out-step<- k new-value)))
    ))

(define (debugger-trap-cont<- k)
  (if (= k halt)
      k
      (make
        ({.take value} (debugging (out-step<- k value)))
        ;; XXX (.inject ...) ?
        (else message (call k message)))))


;; Smoke test

(hide

 (define (try lexp @commands)
   (command-queue .^= commands)
   (let result (interpret lexp))
   (if result (print (survey result)) 'failed))

 (try '(lambda (x) x))
 (try '((lambda (x) ((+ x) 2)) 1))

 (try '((lambda (x) ((+ y) 1)) 42))
 (try '((+ (lambda (z) z)) y))
 (try '(((+ 1) y) 2))

 (try '((lambda (x) ((+ y) 1)) 42)
      {.value 42}
      {.continue})
 (try '((lambda (x) ((+ y) 1)) 42)
      {.value 42}
      {.hop}
      {.b}
      {.hop})
)
