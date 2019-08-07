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

(to (parse lexp)
  (may lexp
    (be (? symbol?)
      (var-ref<- lexp))
    (be (? number?)
      (constant<- lexp))
    (be `(lambda (,v) ,body)
      (abstraction<- v (parse body)))
    (be `(,operator ,operand)
      (call<- (parse operator)
              (parse operand)))))

(to (interpret lexp)
  ((parse lexp) .evaluate global-env halt))


;; ASTs and continuations

(make halt
  (to _.empty? #yes)
  (to (_ .inject k<-) halt)
  (to (_ .take-step val) val)
  (to (_ .take val) val))


;; Constant
(to (constant<- c)
  (make constant
    (to _.source           c)
    (to (_ .eval-step r k) (debugging (value-step<- constant r k)))
    (to (_ .evaluate r k)  (k .take c))))

;; Variable reference
(to (var-ref<- v)
  (make var-ref
    (to _.source
      v)
    (to (_ .eval-step r k)
      (debugging (value-step<- var-ref r k)))
    (to (_ .evaluate r k)
      (lookup r v k))))

;; Lambda expression
(to (abstraction<- v body)
  (make abstraction
    (to _.source
      `(& ,v ,body.source))
    (to (_ .eval-step r k)
      (debugging (value-step<- abstraction r k)))
    (to (_ .evaluate r k)
      (k .take (make _
                 (to _.survey
                   `(,v -> <body>))
                 (to (_ .call arg k2)
                   (body .evaluate (extend r v arg) k2))
                 (to (_ .call-step arg k2)
                   (body .eval-step (extend r v arg) k2)))))))

;; Application
(to (call<- operator operand)
  (make app
    (to _.source
      `(,operator.source ,operand.source))
    (to (_ .eval-step r k)
      (debugging (subeval-step<- operator r (ev-arg-cont<- operand r k))))
    (to (_ .evaluate r k)
      (operator .evaluate r (ev-arg-cont<- operand r k)))))

(to (ev-arg-cont<- operand r k)
  (make (to _.empty? #no)
        (to _.rest k)
        (to _.first `(^ ,operand.source))
        (to (_ .inject k<-) (ev-arg-cont<- operand r (k<- k)))
        (to (_ .take fn)
          (operand .evaluate r (call-cont<- fn k)))
        (to (_ .take-step fn)
          (operand .eval-step r (call-cont<- fn k)))
        ))

(to (call-cont<- fn k)
  (make (to _.empty? #no)
        (to _.rest k)
        (to _.first `(,(survey fn) ^))
        (to (_ .inject k<-) (call-cont<- fn (k<- k)))
        (to (_ .take arg)
          (fn .call arg k))
        (to (_ .take-step arg)
          (fn .call-step arg k))
        ))


;; Built-in values

(to (survey value)
  (if (or (number? value) (symbol? value))
      value
      value.survey))

(make prim+
  (to _.survey '+)
  (to (_ .call-step arg1 k1)
    XXX)
  (to (_ .call arg1 k1)
    (if (number? arg1)
        (k1 .take (make _
                    (to _.survey
                      `(+ ,(survey arg1)))
                    (to (_ .call-step arg2 k2)
                      XXX)
                    (to (_ .call arg2 k2)
                      (if (number? arg2)
                          (k2 .take (+ arg1 arg2))
                          ;; XXX should supply self, too:
                          (debug k2 "Bad arg2 to +" (survey arg2))))))
        (debug k1 "Bad arg1 to +" (survey arg1)))))


;; Environments

(let global-env
  `((+ ,prim+)))

(to (extend r v val)
  `((,v ,val) ,@r))

(to (lookup r v k)
  (may (assoc v r)
    (be #no    (debug k "Unbound var" v))
    (be record (k .take (record 1)))))


;; Debugger
;; Instead of interacting at a prompt, it takes a list of commands,
;; for now, for ease of rerunning during development.

(let command-queue (box<- '()))

(to (next-command)
  (display "debug> ")
  (may command-queue.^
    (be '()
      (newline)
      #no)
    (be `(,first ,@rest)
      (print first)
      (command-queue .^= rest)
      first)))

(to (debug k plaint irritant)
  (complain plaint irritant)
  (traceback k)
  (debugging (out-step<- k 'default-error-value)))

(to (complain plaint irritant)
  (display "Lambdaterp error: ")
  (write plaint)
  (display ": ")
  (write irritant)
  (newline))

(to (traceback k)
  (each! print k))

(to (debugging state)
  (let cmd (next-command))
  (if cmd (call state cmd) #no))

(to (value-step<- e r k)
  (make value-step
    (to _.show
      (display "ev-> ") (print e.source))
    (to _.b
      (traceback k)
      (debugging value-step))
    (to _.continue
      (e .evaluate r k))
    (to _.hop
      (e .evaluate r (k .inject debugger-trap-cont<-)))
    (to _.step
      value-step.hop)
    ))

(to (subeval-step<- e r k)
  (make subeval-step
    (to _.show
      (display "ev-> ") (print e.source))
    (to _.b
      (traceback k)
      (debugging subeval-step))
    (to _.continue
      (e .evaluate r k))
    (to _.hop
      (e .evaluate r (k .inject debugger-trap-cont<-)))
    (to _.step
      (e .eval-step r k))
    ))

(to (out-step<- k value)
  (make out-step
    (to _.show
      (display "<-ret ") (print (survey value)))
    (to _.b
      (traceback k)
      (debugging out-step))
    (to _.continue
      (k .take value))
    (to _.hop
      ((k .inject debugger-trap-cont<-) .take value))
    (to _.step 
      (k .take-step value))
    (to (_ .value new-value)
      (debugging (out-step<- k new-value)))
    ))

(to (debugger-trap-cont<- k)
  (if (= k halt)
      k
      (make _
        (to (_ .take value) (debugging (out-step<- k value)))
        ;; XXX (.inject ...) ?
        (to message (call k message)))))


;; Smoke test

(hide

 (to (try lexp @commands)
   (command-queue .^= commands)
   (let result (interpret lexp))
   (if result (print (survey result)) 'failed))

 (try '(lambda (x) x))
 (try '((lambda (x) ((+ x) 2)) 1))

 (try '((lambda (x) ((+ y) 1)) 42))
 (try '((+ (lambda (z) z)) y))
 (try '(((+ 1) y) 2))

 (try '((lambda (x) ((+ y) 1)) 42)
      (_ .value 42)
      _.continue)
 (try '((lambda (x) ((+ y) 1)) 42)
      (_ .value 42)
      _.hop
      _.b
      _.hop)
)
