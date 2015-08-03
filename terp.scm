;; Scheme subset except with a fancier LAMBDA making objects, with
;; primitives being objects, and with LETREC specified more tightly.

;; Core syntax, first cut:
;; e = v
;;   | (QUOTE constant)
;;   | (MAKE ((QUOTE cue) (v ...) e) ...)   [make object]
;;   | (LETREC ((v e) ...) e)
;;   | ((QUOTE cue) e e ...)    [call method of object]


;; Running a program

(define (run-load filename)
  (call-with-input-file filename
    (lambda (port)
      (let loading ((form (read port)))
        (cond ((not (eof-object? form))
               (run form)
               (loading (read port))))))))

(define (run form)
  (cond ((and (pair? form) (eq? (car form) 'define))
         (run-define (cadr form) (cddr form)))
        ((and (pair? form) (eq? (car form) 'load))
         (run-load (cadr form)))
        (else
         (interpret form))))

(define (run-define head body)
  (if (symbol? head)
      (global-def! head (interpret (car body)))
      (global-def! (car head) (interpret `(lambda ,(cdr head) ,@body)))))

(define (interpret e)
  (evaluate (elaborate e) '()))


;; Expanding out syntactic sugar

(define (elaborate e)
  (cond ((symbol? e) e)
        ((self-evaluating? e) (list 'quote e))
        (else
         (assert (pair? e) "Bad syntax" e)
         (case (car e)
           ((quote)
            e)
           ((begin)
            (elaborate-seq (cdr e)))
           ((make)
            (let ((make<-
                   (lambda (methods)
                     `(make ,@(map (lambda (method)
                                     `(,(car method) ,(cadr method)
                                       ,(elaborate-seq (cddr method))))
                                   methods)))))
              (if (and (not (null? (cdr e)))
                       (symbol? (cadr e)))
                  (elaborate `(letrec ((,(cadr e) ,(make<- (cddr e))))
                                ,(cadr e)))
                  (make<- (cdr e)))))
           ((letrec)
            (if (null? (cadr e))
                (elaborate-seq (cddr e))
                `(letrec ,(map (lambda (defn)
                                 (list (car defn) (elaborate (cadr defn))))
                               (cadr e))
                   ,(elaborate-seq (cddr e)))))
           ((let)
            (elaborate `('run (make ('run ,(map car (cadr e))
                                      ,@(cddr e)))
                              ,@(map cadr (cadr e)))))
           ((lambda)
            (elaborate `(make ('run ,(cadr e) ,@(cddr e)))))
           ((if)
            (let ((test (cadr e)) (if-true (caddr e)) (if-false (cadddr e)))
              ;; XXX ought to coerce test to boolean
              (elaborate `('choose ,test 
                                   (lambda () ,if-true)
                                   (lambda () ,if-false)))))
           ;; ...
           (else
            (if (and (pair? (car e)) (eq? (caar e) 'quote))
                (cons (car e) (map elaborate (cdr e)))
                (cons ''run (map elaborate e))))))))  ; default cue

(define (elaborate-seq es)
  (begin<- (map elaborate es)))

(define (begin<- es)
  (cond ((null? es) ''#f)
        ((null? (cdr es)) (car es))
        (else (begin2<- (car es) (begin<- (cdr es))))))

(define (begin2<- e1 e2)
  `('run (make ('run (v thunk) ('run thunk)))
         ,e1
         (make ('run () ,e2))))

(define (self-evaluating? x)
  (or (boolean? x)
      (number? x)
      (char? x)
      (string? x)))


;; Smoke test of elaboration

(define (assert ok? plaint culprit)
  (if (not ok?)
      (error plaint culprit)
      'fuck-off-mzscheme))

(define (should= x expected)
  (assert (equal? x expected) "Expected" expected))

(should= (elaborate '42)
         ''42)
(should= (elaborate '#f)
         ''#f)
(should= (elaborate ''())
         ''())
(should= (elaborate '(begin (write x) (lambda (y) ('+ x y))))
         '('run
           (make ('run (v thunk) ('run thunk)))
           ('run write x)
           (make ('run () (make ('run (y) ('+ x y)))))))
(should= (elaborate '(begin (if x y z)))
         '('choose x (make ('run () y)) (make ('run () z))))
(should= (elaborate '(lambda ()
                      (letrec ((for-each
                                (lambda (f xs)
                                  (if (null? xs)
                                      '()
                                      (begin (f ('car xs))
                                             (for-each f ('cdr xs)))))))
                        for-each)))
         '(make
           ('run ()
             (letrec ((for-each
                       (make
                        ('run (f xs)
                          ('choose
                           ('run null? xs)
                           (make ('run () '()))
                           (make
                            ('run ()
                              ('run
                               (make ('run (v thunk) ('run thunk)))
                               ('run f ('car xs))
                               (make
                                ('run () ('run for-each f ('cdr xs))))))))))))
               for-each))))


;; Objects, calling, and answering

(define object-tag (list '*object))

(define (object<- script datum)
  (list object-tag script datum))

(define object.script cadr)
(define object.datum caddr)

(define (unwrap x receiver)
  (cond ((boolean? x)   (receiver boolean-script x))
        ((number? x)    (receiver number-script x))
        ((symbol? x)    (receiver symbol-script x))
        ((null? x)      (receiver nil-script x))
        ((char? x)      (receiver char-script x))
        ((string? x)    (receiver string-script x))
        ((vector? x)    (receiver vector-script x))
        ((procedure? x) (receiver scheme-procedure-script x))
        (else
         (assert (pair? x) "Non-object" x)
         (if (eq? (car x) object-tag)
             (receiver (object.script x) (object.datum x))
             (receiver pair-script x)))))

(define (call selector object arguments k)
  (unwrap object
          (lambda (script datum)
            (cond ((assoc selector script) ;TODO assq when memoized
                   => (lambda (pair)
                        (apply (cadr pair) k datum arguments)))
                  (else (signal k "No method found" selector object))))))

(define (selector<- cue arity) (cons cue arity))

;; TODO: special-case this path through call for efficiency
(define (answer k value)
  (call answer/1 k (list value) 'ignored))

(define answer/1 (selector<- 'answer 1))

(define (signal k plaint . values)
  (apply error plaint values))


;; A small-step interpreter

(define (evaluate e r)
  (ev e r halt-cont))

(define (cont-script<- handler)
  `((,answer/1 ,(lambda (_ datum value)
                  (apply handler value datum)))))

(define (cont<- script . data)
  (object<- script data))

(define halt-cont (cont<- (cont-script<- (lambda (value) value))))

(define (ev e r k)
  (if (symbol? e)
      (env-lookup r e k)
      (case (car e)
        ((quote)
         (answer k (cadr e)))
        ((make)
         (answer k (object<- (map (lambda (method)
                                    (list (selector<- (cadar method)
                                                      (length (cadr method)))
                                          (lambda (k r . arguments)
                                            (ev (caddr method)
                                                (env-extend r
                                                            (cadr method)
                                                            arguments)
                                                k))))
                                  (cdr e))
                             r)))
        ((letrec)
         (ev-letrec (cadr e) (caddr e)
                    (env-extend-promises r (map car (cadr e)))
                    k))
        (else
         (ev (cadr e) r
             (cont<- ev-operands-cont-script
                     (cddr e) r
                     (selector<- (cadar e) (length (cddr e)))
                     k))))))

(define ev-operands-cont-script
  (cont-script<-
   (lambda (receiver operands r selector k)
     (if (null? operands)
         (call selector receiver '() k)
         (ev (car operands) r
             (cont<- ev-remaining-operands-cont-script
                     (cdr operands) '() r selector receiver k))))))

(define ev-remaining-operands-cont-script
  (cont-script<-
   (lambda (argument operands arguments r selector receiver k)
     (if (null? operands)
         (call selector receiver (reverse (cons argument arguments)) k)
         (ev (car operands) r
             (cont<- ev-remaining-operands-cont-script
                     (cdr operands) (cons argument arguments)
                     r selector receiver k))))))

(define (ev-letrec defns body new-r k)
  (ev (cadar defns) new-r
      (cont<- letrec-cont-script defns body new-r k)))

(define letrec-cont-script
  (cont-script<-
   (lambda (value defns body new-r k)
     (env-resolve! new-r (caar defns) value)
     (if (null? (cdr defns))
         (ev body new-r k)
         (ev (cadadr defns) new-r
             (letrec-cont<- (cdr defns) body new-r k))))))


;; Environments

(define (env-lookup r v k)
  (define (succeed pair) (answer k (cadr pair)))
  (cond ((assq v r) => succeed)
        ((assq v the-global-env) => succeed)
        (else (signal k "Unbound variable" v))))

(define (env-extend r vs values)
  (append (map list vs values) r))

(define (env-extend-promises r vs)
  (env-extend r vs (map (lambda (_) uninitialized) vs)))

(define (env-resolve! r v value)
  (cond ((assq v r) => (lambda (pair)
                         (assert (eq? (cadr pair) uninitialized) "WTF?" pair)
                         (set-car! (cdr pair) value)))
        (else (error "Can't happen" v))))

(define uninitialized (object<- '() '*uninitialized*))

(define (global-def! name value)
  (set! the-global-env (cons (list name value) the-global-env)))


;; Primitive types and functions
;; Many needn't be primitive, but are for the sake of nicely
;; interfacing with the host language.
;; TODO: signal any errors instead of panicking

(define (primitive-script<- entries)
  (map (lambda (entry)
         (apply (lambda (cue arity procedure)
                  (list (selector<- cue arity) (prim<- procedure)))
                entry))
       entries))

(define (prim<- procedure)
  (lambda (k . args)
    (answer k (apply procedure args))))

(define run/0 (selector<- 'run 0))

(define boolean-script
  (cons (list (selector<- 'choose 2)
              (lambda (k me if-true if-false)
                (call run/0 (if me if-true if-false) '() k)))
        (primitive-script<-
         `((type 0 ,(lambda (k me) (answer k 'boolean)))))))

(define number-script
  (primitive-script<-
   `((type      0 ,(lambda (me) 'number))
     (+         1 ,+)
     (-         1 ,-)
     (*         1 ,*)
     (quotient  1 ,quotient)
     (remainder 1 ,remainder)
     (<         1 ,<)
     (=         1 ,=)
     )))

(define symbol-script
  (primitive-script<-
   `((type   0 ,(lambda (me) 'symbol))
     (name   0 ,symbol->string))))

(define nil-script
  (primitive-script<-
   `((type   0 ,(lambda (me) 'nil))
     (count  0 ,length)
     (run    1 ,list-ref))))

(define char-script
  (primitive-script<-
   `((type   0 ,(lambda (me) 'char)))))

(define pair-script
  (primitive-script<-
   `((type   0 ,(lambda (me) 'pair))
     (first  0 ,car)
     (rest   0 ,cdr)
     (count  0 ,length)
     (run    1 ,list-ref))))

(define string-script
  (primitive-script<-
   `((type   0 ,(lambda (me) 'string))
     (count  0 ,string-length)
     (run    1 ,string-ref))))

(define vector-script
  (primitive-script<-
   `((type   0 ,(lambda (me) 'vector))
     (count  0 ,vector-length)
     (run    1 ,vector-ref)
     (set!   2 ,(lambda (me i value)
                  (vector-set! me i value)
                  #f)))))

(define scheme-procedure-script
  (primitive-script<-
   `((type   0 ,(lambda (me) 'procedure))
     ;;XXX should only define arities that work:
     (run    0 ,(lambda (me) (me)))
     (run    1 ,(lambda (me x) (me x)))
     (run    2 ,(lambda (me x y) (me x y))))))

(define (is? x y)
  (unwrap x (lambda (x-script x-datum)
              (unwrap y (lambda (y-script y-datum)
                          (and (eq? x-script y-script)
                               (eqv? x-datum y-datum)))))))

(define the-global-env
  `((cons ,cons)
    (is? ,is?)
    (symbol? ,symbol?)
    (write ,write)
    (newline ,newline)))


;; Smoke test of evaluation

(should= (interpret '42)
         42)
(should= (interpret 'cons)
         cons)
(should= (interpret '('- 5 3))
         2)
(should= (interpret '('first ('rest '(hello world))))
         'world)
(should= (interpret '('run (make ('run (x) x))
                           '55))
         55)
(should= (interpret '(let ((x (is? 4 ('+ 2 2))))
                       x))
         #t)
(should= (interpret '(letrec ((fact (lambda (n)
                                      (if (is? n 0)
                                          1
                                          ('* n (fact ('- n 1)))))))
                       (fact 5)))
         120)
(should= (interpret '(letrec ((even? (lambda (n)
                                       (if (is? n 0)
                                           #t
                                           (odd? ('- n 1)))))
                              (odd? (lambda (n)
                                      (if (is? n 0)
                                          #f
                                          (even? ('- n 1))))))
                       (even? 5)))
         #f)
