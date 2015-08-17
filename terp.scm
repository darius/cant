;; Interpreter

;; Running a program

(define (boot)
  (run-load "traceback.scm"))

(define (run-load filename)
  (evaluate (elaborate-top-level (snarf filename))
            '()))

(define (interpret e)
  (evaluate (elaborate-expression e) '()))


;; Objects, calling, and answering

(define-structure object script datum)
(define object<- make-object)
(define object.script object-script)
(define object.datum  object-datum)

(define (unwrap x receiver)
  (cond ((boolean? x)   (receiver boolean-script x))
        ((number? x)    (receiver number-script x))
        ((symbol? x)    (receiver symbol-script x))
        ((null? x)      (receiver nil-script x))
        ((char? x)      (receiver char-script x))
        ((string? x)    (receiver string-script x))
        ((vector? x)    (receiver vector-script x))
        ((procedure? x) (receiver scheme-procedure-script x))
        ((pair? x)      (receiver pair-script x))
        ((object? x)    (receiver (object.script x) (object.datum x)))
        (else (error "Non-object" x))))
             
(define (call selector object arguments k)
  (unwrap object
          (lambda (script datum)
            (cond ((or (assoc selector script) ;TODO assq when memoized
                       (assq (selector.cue selector) script))
                   => (lambda (pair)
                        (apply (cadr pair) k datum arguments)))
                  ((assq else-selector script)
                   => (lambda (pair)
                        ((cadr pair)
                         k datum (selector.cue selector) arguments)))
                  (else (message-not-found selector object k))))))

(define (message-not-found selector object k)
  (signal k "No method found" selector object))

(define (selector<- cue arity) (cons cue arity))
(define selector.cue car)
(define else-selector (list "else")) ;XXX not private until we can assq above with memoized selectors

(define (else-script<- proc)
  `((,else-selector ,proc)))

;; TODO: special-case this path through call for efficiency
(define (answer k value)
  (call answer/1 k (list value) 'ignored))

(define answer/1 (selector<- '.answer 1))
(define run/0    (selector<- '.run 0))
(define run/2    (selector<- '.run 2))
(define run/3    (selector<- '.run 3))

(define (signal k plaint . values)
  (let ((handler (get-signal-handler)))
    ;; XXX install backstop handler or something, that'd act like:
    ;;     (apply error plaint values))
    (call run/3 handler (list k plaint values) halt-cont)))

(define (prim-script<- wrap-method entries)
  (map (lambda (entry)
         (apply (lambda (cue arity procedure)
                  (list (selector<- cue arity) (wrap-method procedure)))
                entry))
       entries))

(define (prim<- procedure) ; a wrap-method for typical primitive methods
  (lambda (k . args)
    (answer k (apply procedure args))))

(define (get-signal-handler)
  (call run/0 the-signal-handler-box '() halt-cont))

;; The default bare-bones signal handler.
(define (panic k plaint values)           ;XXX should be an object
  (apply error plaint values))


;; A small-step interpreter

(define (evaluate e r)
  (ev e r halt-cont))

(define (cont<- script . data)
  (object<- script data))

(define (answer-script<- to-answer)
  (prim-script<- identity
   `((.answer 1 ,(lambda (_ datum value)
                   (apply to-answer value datum))))))

(define halt-cont
  (cont<-
   (append
    (answer-script<- (lambda (value) value))
    (prim-script<- identity
     (let ((complain (lambda (k _) (signal k "No more frames" halt-cont))))
       `((.first 0 ,complain)
         (.rest  0 ,complain)
         (.run   1 ,complain))))
    (prim-script<- prim<-
     `((.empty? 0 ,(lambda (_) #t))
       (.count  0 ,(lambda (_) 0)))))))

;; Script for nonempty continuations, i.e. ones other than halt-cont.
(define (cont-script<- to-answer to-first)
  (append
   (answer-script<- to-answer)
   (prim-script<- prim<-
    `((.empty? 0 ,(lambda (datum) #f))
      ;; rest/0 gets the enclosing continuation, which is always
      ;; the first element of datum.
      (.rest   0 ,car)
      (.first  0 ,(lambda (data) (apply to-first (cdr data))))
      ))
   (prim-script<- identity
    `((.count 0 ,(lambda (k datum) (signal k "XXX unimplemented")))
      (.run   1 ,(lambda (k datum) (signal k "XXX unimplemented")))
      ))))

(define (ev e r k)
  (if (symbol? e)
      (env-lookup r e k)
      (case (car e)
        ((quote)
         (answer k (cadr e)))
        ((%make)
         ;; TODO: build the object's script at elaboration time.
         (answer k (object<-
                    (map (lambda (method)
                           (cond
                             ((eq? (car method) 'else)
                              (list else-selector
                                    (lambda (k r cue arguments)
                                      (ev (caddr method)
                                          (env-extend r
                                                      (cadr method)
                                                      (list cue arguments))
                                          k))))
                             ((symbol? (cadr method))  ; variable arity
                              (list (car method)
                                    (lambda (k r . arguments)
                                      (ev (caddr method)
                                          (env-extend r
                                                      (list (cadr method))
                                                      (list arguments))
                                          k))))
                              (else
                               (list (selector<- (car method)
                                                 (length (cadr method)))
                                     (lambda (k r . arguments)
                                       (ev (caddr method)
                                           (env-extend r
                                                       (cadr method)
                                                       arguments)
                                           k))))))
                         (cddr e))
                    r)))
        ((%hide)
         (ev (caddr e)
             (env-extend-promises r (cadr e))
             k))
        ((%let)
         (ev (caddr e) r
             (cont<- let-cont-script k
                     r (cadr e) (cadddr e))))
        (else
         (ev (cadr e) r
             (cont<- ev-operands-cont-script k
                     (cddr e) r
                     (selector<- (car e) (length (cddr e)))))))))

(define let-cont-script
  (cont-script<-
   (lambda (value k r var next-e)
     (if (not (eq? var '_))
         (env-resolve! r var value))
     (ev next-e r k))
   (lambda (r var next-e)
     `(begin ,(if (eq? var '_) '^ `(let ,var ^))
             ,next-e))))

(define ev-operands-cont-script
  (cont-script<-
   (lambda (receiver k operands r selector)
     (if (null? operands)
         (call selector receiver '() k)
         (ev (car operands) r
             (cont<- ev-remaining-operands-cont-script k
                     (cdr operands) '() r selector receiver))))
   (lambda (operands r selector)
     `(,(show-selector selector) ^ ,@operands))))

(define (show-selector selector)
  (car selector))

(define (show-value value)
  (list 'unquote (deep-format value)))

(define ev-remaining-operands-cont-script
  (cont-script<-
   (lambda (argument k operands arguments r selector receiver)
     (if (null? operands)
         (call selector receiver (reverse (cons argument arguments)) k)
         (ev (car operands) r
             (cont<- ev-remaining-operands-cont-script k
                     (cdr operands) (cons argument arguments)
                     r selector receiver))))
   (lambda (operands arguments r selector receiver)
     `(,(show-selector selector)
       ,(show-value receiver)
       ,@(map show-value (reverse arguments))
       ^
       ,@operands))))


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
  (assert (list? r) "what's an r?" r)
  (cond ((assq v r) => (lambda (pair)
                         (assert (eq? (cadr pair) uninitialized)
                                 "Multiple definition" v)
                         (set-car! (cdr pair) value)))
        ((null? r)
         (set! the-global-env (cons (list v value) the-global-env)))
        (else (error "Can't happen" v))))

(define uninitialized (object<- '() '*uninitialized*))

;; Primitive types and functions
;; Many needn't be primitive, but are for the sake of nicely
;; interfacing with the host language.
;; TODO: signal any errors instead of panicking

(define boolean-script
  (append
   (prim-script<- identity 
    `((.choose 2 ,(lambda (k me if-true if-false)
                    (call run/0 (if me if-true if-false) '() k)))))
   (prim-script<- prim<-
    `((.type 0 ,(lambda (k me) (answer k 'boolean)))))))

(define call-prim
  (let ((script
         (append
          (prim-script<- identity 
           `((.run 2 ,(lambda (k _ receiver message)
                        ;; XXX dubious design: it's easy to suppose
                        ;; cue defaults to .run here instead.
                        (let ((cue (car message))
                              (arguments (cdr message)))
                          (call/cue cue receiver arguments k))))
             (.run 3 ,(lambda (k _ cue receiver arguments)
                        (call/cue cue receiver arguments k)))))
          (prim-script<- prim<-
           `((.type 0 ,(lambda (me) 'procedure)))))))
    (object<- script #f)))

(define evaluate-prim
  (let ((script
         (append
          (prim-script<- identity 
           `((.run 2 ,(lambda (k me e r)
                        ;; XXX coerce r to an environment
                        (ev (elaborate-expression e) r k)))))
          (prim-script<- prim<-
           `((.type 0 ,(lambda (me) 'procedure)))))))
    (object<- script #f)))

(define number-script
  (prim-script<- prim<-
   `((.type      0 ,(lambda (me) 'number))
     (.+         1 ,+)
     (.-         1 ,-)
     (.*         1 ,*)
     (.quotient  1 ,quotient)
     (.remainder 1 ,remainder)
     (.<         1 ,<)
     (.=         1 ,=)
     ;; (Gambit-specific implementations:)
     (.<<        1 ,arithmetic-shift)
     (.bit-not   0 ,bitwise-not)
     (.bit-and   1 ,bitwise-and)
     (.bit-or    1 ,bitwise-ior)
     (.bit-xor   1 ,bitwise-xor)
     )))

(define (call/cue cue receiver arguments k)
  (call (selector<- cue (length arguments))
        receiver arguments k))

(define symbol-script
  (append
   (prim-script<- prim<-
    `((.type   0 ,(lambda (me) 'symbol))
      (.name   0 ,symbol->string)))
   (else-script<-
    (lambda (k me cue arguments)
      (if (eq? cue '.run)
          (call/cue me (car arguments) (cdr arguments) k)
          (message-not-found (selector<- cue (length arguments))
                             me k))))))

(define nil-script
  (prim-script<- prim<-
   `((.type   0 ,(lambda (me) 'nil))
     (.empty? 0 ,null?)
     (.count  0 ,length)
     (.run    1 ,list-ref)
     (.chain  1 ,(lambda (me seq) seq)))))

(define pair-script
  (prim-script<- prim<-
   `((.type   0 ,(lambda (me) 'pair))
     (.empty? 0 ,null?)
     (.first  0 ,car)
     (.rest   0 ,cdr)
     (.count  0 ,length)
     (.run    1 ,list-ref)
     (.chain  1 ,append))))

(define char-script
  (prim-script<- prim<-
   `((.type          0 ,(lambda (me) 'char))
     (.code          0 ,char->integer)          ; better name?
     (.alphabetic?   0 ,char-alphabetic?)
     (.digit?        0 ,char-numeric?)
     (.whitespace?   0 ,char-whitespace?)
     (.alphanumeric? 0 ,(lambda (me) (or (char-alphabetic? me)
                                         (char-numeric? me))))
     )))

(define string-script
  (prim-script<- prim<-
   `((.type   0 ,(lambda (me) 'string))
     (.empty? 0 ,(lambda (me) (= 0 (string-length me))))
     (.first  0 ,(lambda (me) (string-ref me 0)))
     (.rest   0 ,(lambda (me) (substring me 1 (string-length me))))
     (.count  0 ,string-length)
     (.run    1 ,string-ref)
     (.has?   1 ,(lambda (me i) (and (integer? i)
                                     (< -1 i (string-length me)))))
     (.chain  1 ,string-append)
     (.slice  1 ,(lambda (me lo) (substring me lo (string-length me))))
     (.slice  2 ,substring)
     (.parse-int 0 ,string->number)      ;XXX not precisely
     (.parse-int 1 ,string->number)
     )))

;; XXX .has? for all the other collections too

(define vector-script
  (prim-script<- prim<-
   `((.type   0 ,(lambda (me) 'vector))
     (.empty? 0 ,(lambda (me) (= 0 (vector-length me))))
     (.first  0 ,(lambda (me) (vector-ref me 0)))
     (.rest   0 ,(lambda (me) (subvector me 1 (vector-length me))))
     (.count  0 ,vector-length)
     (.run    1 ,vector-ref)
     (.chain  1 ,vector-append)          ; (gambit-specific, I think)
     (.slice  1 ,(lambda (me lo) (subvector me lo (vector-length me))))
     (.slice  2 ,subvector)
     (.set!   2 ,(lambda (me i value)
                   (vector-set! me i value)
                   #f)))))

(define scheme-procedure-script
  (prim-script<- prim<-
   `((.type   0 ,(lambda (me) 'procedure))
     ;;XXX should only define arities that work:
     (.run    0 ,(lambda (me) (me)))
     (.run    1 ,(lambda (me a) (me a)))
     (.run    2 ,(lambda (me a b) (me a b)))
     (.run    3 ,(lambda (me a b c) (me a b c))))))

(define (is? x y)
  (unwrap x (lambda (x-script x-datum)
              (unwrap y (lambda (y-script y-datum)
                          (and (eq? x-script y-script)
                               (eqv? x-datum y-datum)))))))

(define (box<- x)
  (object<- box-script (vector x)))

(define box-script
  (prim-script<- prim<-
   `((.type   0 ,(lambda (datum) 'box))
     (.run    0 ,(lambda (datum) (vector-ref datum 0)))
     (.set!   1 ,(lambda (datum value)
                   (vector-set! datum 0 value)
                   #f)))))

(define (box? x)
  (unwrap x (lambda (script _) (eq? script box-script))))

(define the-signal-handler-box (box<- panic))

(define error-script
  (append
   (prim-script<- prim<-
    `((.type   0 ,(lambda (me) 'procedure))))
   `((.run ,(lambda (k _ plaint . values)
              (apply signal k plaint values))))))

(define error-prim (object<- error-script #f))

(define (prim-write x)
  (write (deep-format x)))

(define (deep-format x)
  (cond ((object? x)
         (format-script (object.script x)))
        ((pair? x)
         (cons (deep-format (car x))
               (deep-format (cdr x))))
        (else x)))                      ;for now

(define (format-script script)
  (define sep "")
  (define (format-entry entry)
    (let ((s sep))
      (set! sep ",")
      (string-append s (format-selector (car entry)))))
  (define (format-selector sel)
    (cond ((eq? sel else-selector) "...")
          ((symbol? sel) (symbol->string sel))
          (else (string-append (symbol->string (car sel))
                               "/"
                               (number->string (cdr sel))))))
  (string->symbol (apply string-append
                         `("[" ,@(map format-entry script) "]"))))

(define the-global-env
  `((cons ,cons)
    (is? ,is?)
    (number? ,number?)
    (symbol? ,symbol?)
    (list? ,(lambda (x) (or (null? x) (pair? x))))
    (char? ,char?)
    (string? ,string?)
    (vector? ,vector?)
    (box? ,box?)
    (box<- ,box<-)
    (symbol<- ,string->symbol)
    (string<-list ,list->string)
    (< ,<)  ;; XXX use 'compare method instead
    (<= ,<=)
    (= ,=)
    (equal? ,equal?)  ;; XXX just a stub for the real treatment of equality
    (vector<-count ,make-vector)
    (not ,not)
    (assq ,assq)  ;; TODO replace with 'real' hashmaps
    (display ,display)
    (write ,prim-write)
    (newline ,newline)
    (pp ,pp)                     ;XXX obviously shouldn't be primitive
    (call ,call-prim)
    (evaluate ,evaluate-prim)
    (error ,error-prim)
    (the-signal-handler-box ,the-signal-handler-box)
    ))


;; Smoke test of evaluation

(should= (interpret '42)
         42)
(should= (interpret 'cons)
         cons)
(should= (interpret '(.- 5 3))
         2)
(should= (interpret '(.first (.rest '(hello world))))
         'world)
(should= (interpret '(.run (make (.run (x) x))
                           '55))
         55)
(should= (interpret '(hide
                      (let x (is? 4 (.+ 2 2)))
                      x))
         #t)
(should= (interpret '(hide
                      (define (fact n)
                        (if (is? n 0)
                            1
                            (.* n (fact (.- n 1)))))
                       (fact 5)))
         120)
(should= (interpret '(hide
                      (define (even? n)
                        (if (is? n 0)
                            #t
                            (odd? (.- n 1))))
                      (define (odd? n)
                        (if (is? n 0)
                            #f
                            (even? (.- n 1))))
                      (even? 5)))
         #f)
(should= (interpret '(evaluate '(.- 5 3) '()))
         2)
(should= (interpret '(hide
                      (let b (box<- 42))
                      (.set! b (.+ (b) 1))
                      (b)))
         43)
