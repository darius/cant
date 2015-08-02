;; Scheme subset except with a fancier LAMBDA making objects, with
;; primitives being objects, and with LETREC specified more tightly.

;; Core syntax, first cut:
;; e = v
;;   | (QUOTE constant)
;;   | (MAKE ((QUOTE selector) (v ...) e) ...)   [make object]
;;   | (LETREC ((v e) ...) e)
;;   | ((QUOTE selector) e e ...)    [call method of object]

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
  (define (def name e)
    (set! the-global-env (cons (list name (interpret e)) the-global-env)))
  (if (symbol? head)
      (def head (car body))
      (def (car head) `(lambda ,(cdr head) ,@body))))

(define (interpret e)
  (evaluate (elaborate e) '()))

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
            (let ((make-make
                   (lambda (methods)
                     `(make ,@(map (lambda (method)
                                     `(,(car method) ,(cadr method)
                                       ,(elaborate-seq (cddr method))))
                                   methods)))))
              (if (and (not (null? (cdr e)))
                       (symbol? (cadr e)))
                  (elaborate `(letrec ((,(cadr e) ,(make-make (cddr e))))
                                ,(cadr e)))
                  (make-make (cdr e)))))
           ((letrec)
            `(letrec ,(map (lambda (defn)
                             (list (car defn) (elaborate (cadr defn))))
                           (cadr e))
               ,(elaborate-seq (cddr e))))
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
                (cons ''run (map elaborate e))))))))  ; default selector

(define (elaborate-seq es)
  (make-begin (map elaborate es)))

(define (make-begin es)
  (cond ((null? es) ''#f)
        ((null? (cdr es)) (car es))
        (else (make-begin2 (car es)
                           (make-begin (cdr es))))))

(define (make-begin2 e1 e2)
  `('run (make ('run (v thunk) ('run thunk)))
         ,e1
         (make ('run () ,e2))))

(define (self-evaluating? x)
  (or (boolean? x)
      (number? x)
      (char? x)
      (string? x)))


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


(define (evaluate e r)
  (if (symbol? e)
      (env-lookup r e)
      (case (car e)
        ((quote)
         (cadr e))
        ((make)
         (make-object (map (lambda (method)
                             (list (cadar method)
                                   (lambda (r . arguments)
                                     (evaluate (caddr method)
                                               (env-extend r
                                                           (cadr method)
                                                           arguments)))))
                           (cdr e))
                      r))
        ((letrec)
         ;; Specified to evaluate left-to-right with a definite
         ;; 'uninitialized' value.
         (let ((new-r (env-extend-promises r (map car (cadr e)))))
           (for-each (lambda (defn)
                       (env-resolve! new-r
                                     (car defn)
                                     (evaluate (cadr defn) new-r)))
                     (cadr e))
           (evaluate (caddr e) new-r)))
        (else
         (call (evaluate (cadr e) r)
               (cadar e)
               (map (lambda (operand) (evaluate operand r))
                    (cddr e)))))))

(define (env-lookup r v)
  (cond ((assq v r) => cadr)
        ((assq v the-global-env) => cadr)
        (else (error "Unbound variable" v))))

(define (env-extend r vs values)
  (append (map list vs values) r))

(define (env-extend-promises r vs)
  (env-extend r vs (map (lambda (_) uninitialized) vs)))

(define (env-resolve! r v value)
  (cond ((assq v r) => (lambda (pair)
                         (assert (eq? (cadr pair) uninitialized) "WTF?" pair)
                         (set-car! (cdr pair) value)))
        (else (error "Can't happen" v))))

(define object-tag (list '*object))

(define (make-object script datum)
  (list object-tag script datum))

(define object.script cadr)
(define object.datum caddr)

(define (unwrap x receiver)
  (cond ((boolean? x) (receiver boolean-script x))
        ((number? x)  (receiver number-script x))
        ((symbol? x)  (receiver symbol-script x))
        ((null? x)    (receiver nil-script x))
        ((char? x)    (receiver char-script x))
        ((string? x)  (receiver string-script x))
        ((vector? x)  (receiver vector-script x))
        ((procedure? x) (receiver scheme-procedure-script x))
        (else
         (assert (pair? x) "Non-object" x)
         (if (eq? (car x) object-tag)
             (receiver (object.script x) (object.datum x))
             (receiver pair-script x)))))

(define (call object selector arguments)
  (unwrap object
          (lambda (script datum)
            (cond ((assq selector script)
                   => (lambda (pair)
                        (apply (cadr pair) datum arguments)))
                  (else (error "No method found" selector object))))))

(define (must-be ok? x)
  (if (ok? x)
      x
      (error "Bad argument type" x)))

(define uninitialized (make-object '() '*uninitialized*))

;; A really half-baked selection of types and methods below, just for
;; a concrete start.

(define boolean-script
  `((type ,(lambda (me) 'boolean))
    (choose ,(lambda (me if-true if-false)
               (call (if me if-true if-false) 'run '())))))

;; XXX In a tiny self-hosting system we'd restrict numbers to fixnums.
;; So, properly, these should signal overflow outside that range.
(define number-script
  `((type ,(lambda (me) 'number))
    (+ ,+)
    (- ,-)
    (* ,*)
    (quotient ,quotient)
    (remainder ,remainder)
    (< ,<)
    (= ,=)
    ))

(define symbol-script
  `((type ,(lambda (me) 'symbol))
    (name ,symbol->string)))

(define nil-script
  `((type ,(lambda (me) 'nil))))

(define char-script
  `((type ,(lambda (me) 'char)))) 

;; Pairs are primitive for the sake of nicely interfacing with the host language
(define pair-script
  `((type ,(lambda (me) 'pair))
    (car  ,car)
    (cdr  ,cdr)))

(define string-script
  `((type   ,(lambda (me) 'string))
    (length ,string-length)
    (run    ,string-ref)))

;; NB for a compiler all we badly need is a mutable box type, not full vectors
(define vector-script
  `((type   ,(lambda (me) 'vector))
    (length ,vector-length)
    (run    ,vector-ref)
    (put!   ,(lambda (me i value)
               (vector-set! me i value)
               #f))))

(define scheme-procedure-script
  `((type ,(lambda (me) 'procedure))
    (run ,(lambda (me . args) (apply me args)))))

(define (the-eq? x y)
  (unwrap x (lambda (x-script x-datum)
              (unwrap y (lambda (y-script y-datum)
                          (and (eq? x-script y-script)
                               (eqv? x-datum y-datum)))))))

(define the-global-env
  `((cons ,cons)
    (eq? ,the-eq?)
    (symbol? ,symbol?)
    (write ,write)
    (newline ,newline)))


(should= (interpret '42)
         42)
(should= (interpret 'cons)
         cons)
(should= (interpret '('- 5 3))
         2)
(should= (interpret '('car ('cdr '(hello world))))
         'world)
(should= (interpret '('run (make ('run (x) x))
                           '55))
         55)
(should= (interpret '(let ((x (eq? 4 ('+ 2 2))))
                       x))
         #t)
(should= (interpret '(letrec ((fact (lambda (n)
                                      (if (eq? n 0)
                                          1
                                          ('* n (fact ('- n 1)))))))
                       (fact 5)))
         120)
