;; Bootstrap prereqs

(define-record-type object (fields script datum))   ; Nonprimitive objects, that is.
(define object<- make-object)

(define-record-type script (fields name trait clauses))
(define script<- make-script)

;; Parser bootstrap

(define (expression<- vec) (object<- expression-script vec))
(define (pattern<- vec)    (object<- pattern-script vec))

(define (__expr thing)
  (assert (and (vector? thing)
               (< 0 (vector-length thing))
               (number? (vector-ref thing 0)))
          "Must be an internal expression" thing)
  (expression<- thing))

(define (__patt thing)
  (assert (and (vector? thing)
               (< 0 (vector-length thing))
               (number? (vector-ref thing 0)))
          "Must be an internal pattern" thing)
  (pattern<- thing))

(define (parse-exp e . opt-context)
  (let* ((parsed (parse-e e (optional-context 'parse-exp opt-context)))
         (vars (exp-vars-defined parsed))
         (e (elaborate-e parsed (env-extend '() vars (obliviate vars #t)))))
    (expression<- e)))

(define (parse-pat p . opt-context)
  (let* ((parsed (parse-p p (optional-context 'parse-pat opt-context)))
         (vars (pat-vars-defined parsed))
         (p (elaborate-p parsed (env-extend '() vars (obliviate vars #t)))))
    (pattern<- p)))

(define (load-ast-script script-name module-name)
  (let ((form (car (snarf (string-append module-name ".scm") squeam-read))))
    (let ((e (parse-e form `(,module-name))))
      ;; This stands in for calling `evaluate`, which we don't have yet:
      (assert (equal? (pack-tag e) e-make) "Script must be `make`" e)
      (unpack e (name trait clauses)
;;        (assert (eq? stamp none-exp) "simple script" e)
        (assert (eq? trait none-exp) "simple script" e)
        (let ((prim (object<- (script<- name #f clauses)
                              primitive-env)))
          (script<- 'script-name prim primitive-env))))))

(define primitive-env '())

(define expression-script
  (load-ast-script 'expression-primitive "terp/ast-expression"))

(define pattern-script
  (load-ast-script 'pattern-primitive "terp/ast-pattern"))

(define (unwrap-ast ast)
  (assert (and (object? ast)
               (or (eq? (object-script ast) expression-script)
                   (eq? (object-script ast) pattern-script)))
          "Not an ast object" ast)
  (object-datum ast))

(define (__ast-part ast i) (vector-ref (unwrap-ast ast) i))
(define (__ast-tag ast)    (__ast-part ast 0))
(define (__ast-e ast i)    (expression<- (__ast-part ast i)))
(define (__ast-p ast i)    (pattern<- (__ast-part ast i)))
(define (__ast-es ast i)   (map expression<- (__ast-part ast i)))
(define (__ast-ps ast i)   (map pattern<- (__ast-part ast i)))
(define (__ast-clauses ast i)
  (map (lambda (clause)
         `(,(pattern<- (car clause))
           ,(cadr clause)
           ,(caddr clause)
           ,(expression<- (cadddr clause))))
       (__ast-part ast i)))

;; Interpreter

(define (run-load filename)
  (let ((forms (snarf filename squeam-read)))
    (squeam-interpret `(do ,@forms))))

(define (squeam-interpret e)
  (evaluate (parse-exp e) repl-env))

(define repl-env '())

(define (dbg x)
;  (pp x))
  #f)

(define (set-dbg! debug?)
  (set! dbg (if debug? pp (lambda (x) #f))))


;; Hashing and equality

;; For now, I'm gonna assume Squeam-defined objects are equal iff
;; eq?. This means you can't reconstitute an object from its script
;; and datum, which would be a reasonable implementation-level
;; operation for which squeam=? would check if script and datum are
;; eq?, and hashing would also have to look at both.

;; XXX above comments irrelevant since switch from Gambit to Chez.
;; needs a radical overhaul to use Chez's eq? hashtables

(define (hash x)
  (equal-hash x))        ;XXX semantically wrong for Squeam. quick hack to try out Chez.
;  (cond ((term? x) (hash-term x))
;        ((pair? x) (hash-pair x))
;        ((string? x) (hash-string x))
;        (else (equal-hash x)))) 

(define (hash-term x)
  1) ;(hash-em 1 (cons (term-tag x) (term-parts x))))

(define (hash-pair x)
  2) ;(hash-em 2 (list (car x) (cdr x))))

(define (hash-string x)
  3) ; (hash-em 3 (string->list x)))         ;TODO find a built-in string hash fn?

(define (hash-em seed xs)
  (foldl hash-mix seed xs))

(define (hash-mix h x)
  (+ (* 7 h) (hash x))) ;XXX we want a function that mixes nicely into the low-order bits

(define (squeam=? x y)
  (cond ((term? x) (and (term? y) (term=? x y)))
        ((pair? x) (and (pair? y) (pair=? x y)))
        ((string? x) (and (string? y) (string=? x y)))
        (else (eqv? x y))))

(define (pair=? x y)
  (and (squeam=? (car x) (car y))
       (squeam=? (cdr x) (cdr y))))

(define (term=? x y)
  (and (squeam=? (term-tag x) (term-tag y))
       (let ((xs (term-parts x))
             (ys (term-parts y)))
         (and (= (length xs) (length ys))
              (let checking ((xs xs) (ys ys))
                (or (null? xs)
                    (and (squeam=? (car xs) (car ys))
                         (checking (cdr xs) (cdr ys)))))))))


;; Compare primitives

(define (char-compare x y)
  (and (char? x) (char? y)      ;; XXX raise an error instead?
       (cond ((char<? x y) -1)
             ((char=? x y)  0)
             (else            +1))))

(define (number-compare x y)
  (and (number? x) (number? y)      ;; XXX raise an error instead?
       (cond ((< x y) -1)
             ((= x y)  0)
             (else    +1))))

(define (string-compare x y)
  (and (string? x) (string? y)      ;; XXX raise an error instead?
       (cond ((string<? x y) -1)
             ((string=? x y)  0)
             (else            +1))))


;; Objects, calling, and answering

(define-record-type cps-script (fields name procedure))
(define cps-script<- make-cps-script)

;; For less inefficiency, a continuation is just a list, whose car
;; element is the continuation procedure. This representation must
;; never be exposed directly to Squeam code, only wrapped as an object
;; with an appropriate wrapper script. TODO see if a vector or record
;; is better than a list -- probably.
(define (answer k value)
;  (dbg `(answer ,value ,k))
;  (dbg `(answer))
  ((vector-ref k 0) value k))

(define (call object message k)
;  (dbg `(call))
  (cond
   ((object? object)
    (let ((script (object-script object))
          (datum (object-datum object)))
      (cond ((script? script)
             (run-script object script datum message k))
            ((cps-script? script)
             (if (or (pair? message) (null? message)) ;XXX also check for matching arity
                 ((cps-script-procedure script) datum message k)
                 (delegate (get-prim 'cps-primitive)
                           object message k)))
            ((cont-script? script)
             (if (and (term? message)
                      (eq? '.answer (term-tag message))
                      (= 1 (length (term-parts message))))
                 (let ((unwrapped-k (list->vector (cons (cont-script-answerer script) datum))))
                   ((cont-script-answerer script)
                    (car (term-parts message))
                    unwrapped-k))
                 (delegate (get-prim (cont-script-name script))
                           object message k)))
            (else
             (error 'call "Not a script" script datum)))))
   ((procedure? object)
    (if (or (pair? message) (null? message))
        ;; Intercept Scheme-level errors:
        (call/cc
         (lambda (scheme-cont)
           (answer k
                   (with-exception-handler
                    (lambda (exc)
                      (scheme-cont
                       (if (condition? exc)
                           (let ((plaint
                                  (with-output-to-string
                                    (lambda () (display-condition exc)))))
                             (signal k plaint object message))
                           (signal k "Primitive error" exc object message))))
                    (lambda ()
                      ;; Do the Scheme call in this error-handling context.
                      (apply object message))))))
        (run-script object procedure-script object message k)))
   (else
    (let ((script (extract-script object)))
      (run-script object script object message k)))))

(define (extract-script object)
  (cond
   ((number? object)      number-script)
   ((vector? object)      array-script)
   ((pair? object)        pair-script)
   ((box? object)         box-script)
   ((string? object)      string-script)
   ((null? object)        nil-script)
   ((symbol? object)      symbol-script)
   ((output-port? object) sink-script)
   ((input-port? object)  source-script)
   ((char? object)        char-script)
   ((boolean? object)     boolean-script)
   ((term? object)        term-script)
   ((eq? object (void))   void-script)
   ((script? object)      script-script)
   ;; XXX: cont-script? too
   ((procedure? object)   procedure-script)
   ((object? object)      (object-script object))
   (else (error 'call "Non-object" object))))

(define (extract-datum object)
  (cond
   ((object? object)      (object-datum object))
   ;; XXX: script and cont-script too?
   (else                  object)))

(define (error-prim message)
  (let* ((the-box (get-prim 'the-signal-handler))
         (handler (unbox the-box)))
    ;; Panic by default if another error occurs during error handling.
    ;; (We're not doing this here anymore, because Squeam code is
    ;; supposed to make similar arrangements. But you might want to go
    ;; back to this in ticklish situations still.
;;    (call the-box (term<- '.^= panic) halt-cont)
    ;; OK, up to the handler now.
    (call handler message halt-cont)))

(define error-prim-object
  (object<- (cps-script<- 'error
                          (lambda (datum message k)
                            ;;TODO could *almost* just call signal
                            (error-prim (cons (wrap-cont k) message))))
            #f))

(define (panic message)
  (let ((message-for-chez ;Chez Scheme is picky about arguments to (error).
         (if (and (pair? message) (string? (car message)))
             message
             (cons "Error" message))))
    (apply error 'panic message-for-chez)))

(define panic-object
  (object<- (cps-script<- 'panic
                          (lambda (datum message k)
                            (panic message)))
            #f))

(define (run-script object script datum message k)
  (matching (script-clauses script) object script datum message k))

(define (matching clauses object script datum message k)
;  (dbg `(matching)) ; ,clauses))
  (mcase clauses
    (()
     (delegate (script-trait script) object message k))
    (((pattern pat-vars . body) . rest-clauses)
     (let ((pat-r (env-extend-promises datum pat-vars)))
       (ev-pat message pattern pat-r
               (cont<- match-clause-cont k pat-r body rest-clauses object script datum message))))))  ;XXX geeeez

(define (delegate trait object message k)
;  (dbg `(delegate))
  (let ((handler (cond ((object? trait) trait)
                       ((not trait) miranda-trait)
                       (else (error 'delegating "Unknown trait type" trait)))))
    (call handler (list object message) k)))

(define (signal k plaint . values)
  (error-prim `(,(wrap-cont k) ,plaint ,@values)))

(define (as-cons x)
  (and (pair? x)
       (term<- 'cons (car x) (cdr x))))

(define box<- box)
      
(define evaluate-prim
  (object<- (cps-script<- 'evaluate
                          (lambda (datum message k)
                            ;;XXX check arity
                            (apply evaluate-exp `(,@message ,k))))
            #f))

(define with-ejector
  (object<- (cps-script<- 'with-ejector
                          (lambda (datum message k)
                            ;;XXX check arity
                            (let* ((ejector-k
                                    (cont<- unwind-cont k unwind-ejector #t))
                                   (ejector (ejector<- ejector-k)))
                              (call (car message) (list ejector) ejector-k))))
            #f))

(define (ejector<- ejector-k)
  (object<- ejector-script ejector-k)) ;XXX another place extract-datum could wreak havoc

(define (unwind-cont value k0)
  (unpack k0 (k unwind-action)
    (unwind-action k0 (cont<- replace-answer-cont k value))))

(define (replace-answer-cont value-to-ignore k0)
  (unpack k0 (k value)
    (answer k value)))

;; k0 is like #(unwind-cont parent-k unwind-ejector enabled?)
(define (unwind-ejector k0 k)
  (vector-set! k0 3 #f)
  (answer k (void)))

(define ejector-eject
  (object<- (cps-script<-
             '__eject
             (lambda (datum message k)
               ;;XXX check arity
               (let ((ejector (car message))
                     (value (cadr message)))
                 (if (and (object? ejector)
                          (eq? (object-script ejector) ejector-script))
                     (let ((ejector-k (object-datum ejector)))
                       (assert (vector? ejector-k) "ejector-eject vector"
                               ejector-k)
                       (assert (eq? unwind-cont (vector-ref ejector-k 0))
                               "Ejector cont is a cont" ejector-k)
                       (if (vector-ref ejector-k 3) ;still enabled?
                           (ejector-unwinding k ejector-k value)
                           (signal k "Tried to eject to a disabled ejector"
                                   ejector)))
                     (signal k "Not an ejector" ejector)))))
            #f))

(define (ejector-unwinding k ejector-k value)
  (if (eq? k ejector-k)
      (answer ejector-k value)
      (let ((k-action (vector-ref k 0))
            (parent-k (vector-ref k 1)))
        (if (eq? k-action unwind-cont)
            (let ((unwind-action (vector-ref k 2)))
              (unwind-action k (cont<- keep-unwinding parent-k ejector-k value)))
            (ejector-unwinding parent-k ejector-k value)))))

(define (keep-unwinding value-to-ignore k0)
  (unpack k0 (k ejector-k value)
    (ejector-unwinding k ejector-k value)))

(define (do-ejector-protect datum message k)
  ;;XXX check arity
  (let ((thunk (car message))
        (unwind-thunk (cadr message)))
    (call thunk '() (cont<- unwind-cont k call-unwinder unwind-thunk))))

(define (call-unwinder k0 k)
  (let ((unwind-thunk (vector-ref k0 3)))
    (call unwind-thunk '() k)))

(define ejector-protect                 ;TODO rename
  (object<- (cps-script<- 'ejector-protect do-ejector-protect) #f))


;; Environments

(define (vector-append v1 v2)
  (let ((n1 (vector-length v1))
        (n2 (vector-length v2)))
    (let ((result (make-vector (+ n1 n2))))
      (copy-range! result  0 v1 0 n1)
      (copy-range! result n1 v2 0 n2)
      result)))

(define (subvector v lo hi)
  (let ((n (max 0 (- hi lo))))
    (let ((result (make-vector n)))
      (copy-range! result 0 v lo n)
      result)))
  
(define (copy-range! dest d source s n)
  (do ((i (- n 1) (- i 1)))
      ((< i 0))
    (vector-set! dest (+ d i)
                 (vector-ref source (+ s i)))))

(define (prim-display x . opt-sink)
  (let ((sink (cond ((null? opt-sink) (current-output-port))
                    ((null? (cdr opt-sink)) (car opt-sink))
                    (else (error 'prim-display "Too many arguments" `(,x ,@opt-sink))))))
    (cond ((or (char? x) (string? x) (symbol? x) (number? x))
           (display x sink))
          ((boolean? x) ;just for completeness -- not sure I want this
           (display (if x "#yes" "#no") sink))
          (else
           (display "#<XXX non-basic display>" sink))))) ;TODO

(define (prim-write x sink)
  (let ((s (depict x)))
    (cond ((output-port? sink)
           (display s sink))
          (else
           ;;XXX shouldn't call Squeam from a Scheme primitive
           (call sink (term<- '.display s) halt-cont)))))

(define (depict x)
  (cond ((object? x)
         (string-append "#<"
                        (let ((script (object-script x)))
                          (cond ((script? script)
                                 (script-name script))
                                ((cont-script? script)
                                 (cont-script-name script))
                                (else "XXX-WTF")))
                        ">"))
        (else
         ;;XXX other types specially? booleans at least?
         (call-with-string-output-port
          (lambda (p) (put-datum p x))))))

(define the-global-env
  `((__as-cons ,as-cons)
    (= ,squeam=?)
    (out ,(current-output-port))
    (stdin ,(current-input-port))       ;XXX inconsistent

    (cons ,cons)
    (null? ,null?)
    (cons? ,pair?)
    (list? ,(lambda (x) (or (null? x) (pair? x))))
    (number? ,number?)
    (integer? ,integer?)
    (symbol? ,symbol?)
    (cue? ,cue?)
    (claim? ,boolean?)
    (char? ,char?)
    (string? ,string?)
    (array? ,vector?)
    (box? ,box?)
    (term? ,term?)
    (source? ,input-port?)
    (sink? ,output-port?)
    (eof? ,eof-object?)
    (box<- ,box<-)
    (symbol<- ,string->symbol)
    (term<- ,make-term)       ;TODO check that arguments arg is a list
    (char<- ,integer->char)
    (string<-list ,list->string)
    (array<-count ,make-vector)
    (exact->inexact ,exact->inexact)  ;XXX rename or something
    (not ,not)
    (assoc ,assoc)  ;; TODO replace with 'real' hashmaps
    (sqrt ,sqrt)
    (display ,prim-display)
    (newline ,newline)           ;XXX temp
    (panic ,panic-object)
    (error ,error-prim-object)
    (evaluate ,evaluate-prim)
    (open-input-file ,open-input-file)
    (open-output-file ,open-output-file)
    (__set-dbg! ,set-dbg!)
    (with-ejector ,with-ejector)
    (__eject ,ejector-eject)
    (ejector-protect ,ejector-protect)

    ;; These will get high-level definitions later TODO
    (void ,(void))
    (/ ,/)
    (expt ,expt)
    (abs ,abs)
    (gcd ,gcd)
    (array<-list ,list->vector)
    (read ,squeam-read)
    (parse-exp ,parse-exp)
    (parse-pat ,parse-pat)
    (system ,system)
    ;; Should use string ports instead:
    (number<-string ,string->number)
    (string<-number ,number->string)
    (list<-string ,string->list)
    (read ,squeam-read)
    (string-source<- ,open-input-string)
    (string-sink<- ,open-output-string)
    (__get-output-string ,get-output-string)
    (self-evaluating? ,self-evaluating?)
    (maybe-macroexpand-expr ,(lambda (e)
                               (cond ((and (pair? e) (look-up-macro (car e)))
                                      => (lambda (expander)
                                           (term<- 'ok (expander e))))
                                     (else #f))))
    (maybe-macroexpand-patt ,(lambda (e)
                               (cond ((and (pair? e) (look-up-pat-macro (car e)))
                                      => (lambda (expander)
                                           (term<- 'ok (expander e))))
                                     (else #f))))
    (open-subprocess ,process)
    (list-globals ,(lambda () (map car the-global-env)))
    (extract-script ,extract-script)
    (extract-datum ,extract-datum)
    (__halp-log ,(lambda (start end result)
                   (format #t "Halp ~w..~w: ~w\n" start end result) ;TODO actual format
                   result))

    (nano-now ,(lambda ()
                 (let ((t (current-time)))
                   (+ (* 1000000000 (time-second t))
                      (time-nanosecond t)))))
    (nanosleep ,(lambda (nsec)
                  ;; XXX untested
                  (let* ((n (modulo nsec 1000000000))
                         (nsec (- nsec n)))
                    (sleep (make-time 'time-duration n (quotient nsec 1000000000))))))

    ;; Primitives only -- TODO seclude in their own env:
    (__hash ,hash)
    (__char-compare ,char-compare)
    (__number-compare ,number-compare)
    (__+ ,+)
    (__- ,-)
    (__* ,*)
    (__quotient ,quotient)
    (__remainder ,remainder)
    (__modulo ,modulo)
    (__*/mod ,(lambda (n1 n2 d)
                (call-with-values (lambda () (div-and-mod (* n1 n2) d)) list)))
;    (__number-compare
    (__bit-<< ,ash)
    (__bit->> ,(lambda (x y) (ash x (- y))))
    (__bit-not ,lognot)
    (__bit-and ,logand)
    (__bit-or  ,logior)
    (__bit-xor ,logxor)
    (__car ,car)
    (__cdr ,cdr)
    (__append ,append)
    (__length ,length)
    (__list-ref ,list-ref)
    (__append ,append)
    (__symbol->string ,symbol->string)
    (__string-append ,string-append)
    (__string-compare ,string-compare)
    (__string-length ,string-length)
    (__string-maps? ,(lambda (me i)
                       (and (integer? i)
                            (< -1 i (string-length me)))))
    (__string-ref ,string-ref)
    (__substring ,(lambda (me lo bound)
                    (if (< lo (string-length me))
                        (substring me lo (min bound (string-length me)))
                        "")))
    (__vector-append ,vector-append)
    (__vector-copy ,vector-copy)
    (__vector-length ,vector-length)
    (__vector-maps? ,(lambda (me i)
                       (and (integer? i)
                            (< -1 i (vector-length me)))))
    (__vector-ref ,vector-ref)
    (__vector-set! ,vector-set!)
    (__vector->list ,vector->list)
    (__subvector ,subvector)            ;XXX redefine this to result in a view instead of a copy? ditto for strings
    (__char->integer ,char->integer)
    (__char-digit? ,char-numeric?)
    (__char-letter? ,char-alphabetic?)
    (__char-whitespace? ,char-whitespace?)
    (__char-lowercase ,char-downcase)
    (__char-uppercase ,char-upcase)
    (__box-value ,unbox)
    (__box-value-set! ,set-box!)
    (__term-tag ,term-tag)
    (__term-arguments ,term-parts)
    (__close-port ,close-port)
    (__read-char ,read-char)
    (__char-ready? ,char-ready?)
    (__read-all ,(lambda (port)
                   (let reading ((cs '()))
                     (let ((c (read-char port)))
                       (if (eof-object? c)
                           (if (null? cs)
                               c
                               (list->string (reverse cs)))
                           (reading (cons c cs)))))))
    (__write-char ,write-char)
    (__display ,prim-display)
    (__write ,prim-write)

    (__u+ ,(lambda (a b) (logand mask32 (+ a b)))) ;XXX revisit these definitions
    (__s+ ,(lambda (a b) (logand mask32 (+ a b)))) ;XXX I forget what distinction I meant to make
    (__s* ,(lambda (a b) (logand mask32 (* a b))))
    (__u- ,(lambda (a b) (logand mask32 (- a b))))
    (__u<< ,(lambda (a b) (logand mask32 (ash a b))))
    (__u>> ,(lambda (a b) (logand mask32 (ash a (- b)))))

    (__expr     ,__expr)
    (__patt     ,__patt)
    (__ast-part ,__ast-part)
    (__ast-tag  ,__ast-tag)
    (__ast-e    ,__ast-e)
    (__ast-p    ,__ast-p)
    (__ast-es   ,__ast-es)
    (__ast-ps   ,__ast-ps)
    (__ast-clauses ,__ast-clauses)

    (__cps-primitive-name ,(lambda (x)
                             (cps-script-name (object-script x))))
    (__script-name ,script-name)
    (__script-trait ,script-trait)
    (__script-clauses ,script-clauses)
    (__cont-data ,(lambda (wrapped-k)
                    (let ((datum (object-datum wrapped-k)))
                      (cdr datum))))
    (__cont-next-cont ,(lambda (wrapped-k)
                         (let* ((datum (object-datum wrapped-k))
                                (k (car datum)))
                           (wrap-cont k))))
    (os-exit ,exit)
    ))

(define mask32 (- 1 (expt 2 32)))

(define (env-defined? r v)
  (define (succeed pair) #t)  ;XXX or (not (eq? (cadr pair) uninitialized)))
  (cond ((assq v r) => succeed)
        ((assq v the-global-env) => succeed)
        (else #f)))

(define (env-lookup r v k)
  (define (succeed pair) (answer k (cadr pair)))
  (cond ((assq v r) => succeed)
        ((assq v the-global-env) => succeed)
        (else (signal k "Unbound variable" v))))

(define (env-extend r vs values)
  (append (map list vs values) r))

(define (env-extend-promises r vs)
  (env-extend r vs (map (lambda (_) uninitialized) vs)))

(define (env-resolve! r v value k)
  (cond ((assq v r) => (lambda (pair)
                         (if (not (eq? (cadr pair) uninitialized))
                             (signal k "Multiple definition" v)
                             (begin (set-car! (cdr pair) value)
                                    (answer k #t)))))
        ((null? r)
         (cond ((assq v the-global-env)
                => (lambda (pair)
                     ;;XXX as a hack, allow global redefinition for
                     ;; now. This aids development at the repl, but we
                     ;; need a more systematic solution.
                     ;;(signal k "Global redefinition" v)
                     (set-car! (cdr pair) value)
                     (answer k #t)))
               (else
                (set! the-global-env (cons (list v value) the-global-env))
                (answer k #t))))
        (else (signal k "Tried to bind in a non-environment" r v))))

(define uninitialized (object<- (script<- '<uninitialized> #f '()) '*uninitialized*))


;; A small-step interpreter

(define (evaluate e r)
;  (report `(evaluate ,e))
  (evaluate-exp e r halt-cont))

(define (evaluate-exp e r k)
  (if (and (object? e) (eq? (object-script e) expression-script))
      (ev-exp (object-datum e) r k)
      (signal k "evaluate: Not an expression" e)))

(define (ev-exp e r k)
;  (dbg `(ev-exp)) ; ,e))
  ((vector-ref methods/ev-exp (pack-tag e))
   e r k))

(define methods/ev-exp
  (vector
   (lambda (e r k)                          ;e-constant
     (unpack e (value)
       (answer k value)))
   (lambda (e r k)                          ;e-variable
     (unpack e (var)
       (env-lookup r var k)))
   (lambda (e r k)                          ;e-term
     (unpack e (tag es)
       (ev-args es r '()
                (cont<- ev-tag-cont k tag))))
   (lambda (e r k)                          ;e-list
     (unpack e (es)
       (ev-args es r '() k)))
   (lambda (e r k)                          ;e-make
     (unpack e (name trait clauses)
       (if (eq? trait none-exp) ; Just fast-path tuning; this IF is not logically necessary.
           (answer k (object<- (script<- name #f clauses)
                               r))
           (ev-exp trait r
                   (cont<- ev-make-cont-script k r name clauses)))))
   (lambda (e r k)                          ;e-do
     (unpack e (e1 e2)
       (ev-exp e1 r (cont<- ev-do-rest-cont k r e2))))
   (lambda (e r k)                          ;e-let
     (unpack e (p e1)
       (ev-exp e1 r (cont<- ev-let-match-cont k r p))))
   (lambda (e r k)                          ;e-call
     (unpack e (e1 e2)
       (ev-exp e1 r (cont<- ev-arg-cont k r e2))))))

(define (ev-args es r vals k)
  (if (null? es)
      (answer k (reverse vals))
      (ev-exp (car es) r
              (cont<- ev-rest-args-cont k r (cdr es) vals))))

(define (ev-pat subject p r k)
;  (dbg `(match)) ; ,subject ,p))
  ((vector-ref methods/ev-pat (pack-tag p))
   subject p r k))

(define methods/ev-pat
  (vector
   (lambda (subject p r k)              ;p-constant
     (unpack p (value)
       (answer k (squeam=? subject value))))
   (lambda (subject p r k)              ;p-any
     (answer k #t))
   (lambda (subject p r k)              ;p-variable
     (unpack p (name)
       (env-resolve! r name subject k)))
   (lambda (subject p r k)              ;p-term
     (unpack p (tag args)
       (if (not (and (term? subject)
                     (squeam=? (term-tag subject) tag)))
           (answer k #f)
           (ev-match-all (term-parts subject) args r k))))
   (lambda (subject p r k)              ;p-list
     (unpack p (args)
       (if (or (and (pair? args) (pair? subject)
                    (= (length args) (length subject))) ;TODO move this into ev-match-all
               (and (null? args) (null? subject)))      ;TODO answer right away here
           (ev-match-all subject args r k)
           (answer k #f))))
   (lambda (subject p r k)              ;p-and
     (unpack p (p1 p2)
       (ev-pat subject p1 r
               (cont<- ev-and-pat-cont k r subject p2))))
   (lambda (subject p r k)              ;p-view
     (unpack p (e p1)
       (ev-exp e r
               (cont<- ev-view-call-cont k r subject p1))))))

(define (ev-match-all subjects ps r k)
  (cond ((null? ps)
         (answer k (null? subjects)))
        ((null? subjects)
         (answer k #f))
        (else
         (ev-pat (car subjects) (car ps) r
                 (cont<- ev-match-rest-cont k r (cdr subjects) (cdr ps))))))

(define cont<- vector)

(define-record-type cont-script (fields name answerer))

(define (halt-cont-fn value k0) value)
(define halt-cont (cont<- halt-cont-fn))

(define (match-clause-cont matched? k0)
;     (dbg `(match-clause-cont))
  (unpack k0 (k pat-r body rest-clauses object script datum message)
               ;; TODO don't unpack it all till needed
  ;; body is now a list (body-vars body-exp)
    (if matched?
        (ev-exp (cadr body) (env-extend-promises pat-r (car body)) k)
        (matching rest-clauses object script datum message k))))

(define (ev-make-cont-script trait-val k0)
;     (dbg `(ev-make-cont))
  (unpack k0 (k r name clauses)
    (answer k (object<- (script<- name trait-val clauses)
                        r))))

(define (ev-do-rest-cont _ k0)
;     (dbg `(ev-do-rest-cont))
  (unpack k0 (k r e2)
    (ev-exp e2 r k)))

(define (ev-let-match-cont val k0)
;     (dbg `(ev-let-match-cont))
  (unpack k0 (k r p)
    (ev-pat val p r
            (cont<- ev-let-check-cont k val))))

(define (ev-let-check-cont matched? k0)
;     (dbg `(ev-let-check-cont))
  (unpack k0 (k val)
    (if matched?
        (answer k val)
        (signal k "Match failure" val))))

(define (ev-arg-cont receiver k0)
;     (dbg `(ev-arg-cont))
  (unpack k0 (k r e2)
    (ev-exp e2 r
            (cont<- ev-call-cont k receiver))))

(define (ev-call-cont message k0)
;     (dbg `(ev-call-cont ,receiver ,message))
  (unpack k0 (k receiver)
    (call receiver message k)))

(define (ev-rest-args-cont val k0)
;     (dbg `(ev-rest-args-cont))
  (unpack k0 (k r es vals)
    (ev-args es r (cons val vals) k)))

(define (ev-tag-cont vals k0)
;     (dbg `(ev-tag-cont))
  (unpack k0 (k tag)
    (answer k (make-term tag vals))))

(define (ev-and-pat-cont matched? k0)
;     (dbg `(ev-and-pat-cont))
  (unpack k0 (k r subject p2)
    (if matched?
        (ev-pat subject p2 r k)
        (answer k #f))))

(define (ev-view-call-cont convert k0)
;     (dbg `(ev-view-call-cont))
  (unpack k0 (k r subject p)
    (call convert (list subject)
          (cont<- ev-view-match-cont k r p))))

(define (ev-view-match-cont new-subject k0)
;     (dbg `(ev-view-match-cont))
  (unpack k0 (k r p)
    (ev-pat new-subject p r k)))

(define (ev-match-rest-cont matched? k0)
;     (dbg `(ev-match-rest-cont))
  (unpack k0 (k r subjects ps)
    (if matched?
        (ev-match-all subjects ps r k)
        (answer k #f))))


(define (wrap-cont k)
  (let* ((f (vector-ref k 0))
         (name
          (cond
           ((eq? f halt-cont-fn)         '__halt-cont)
           ((eq? f match-clause-cont)    '__match-clause-cont)
           ((eq? f ev-make-cont-script)  '__ev-make-cont)
           ((eq? f ev-do-rest-cont)      '__ev-do-rest-cont)
           ((eq? f ev-let-match-cont)    '__ev-let-match-cont)
           ((eq? f ev-let-check-cont)    '__ev-let-check-cont)
           ((eq? f ev-arg-cont)          '__ev-arg-cont)
           ((eq? f ev-call-cont)         '__ev-call-cont)
           ((eq? f ev-rest-args-cont)    '__ev-rest-args-cont)
           ((eq? f ev-tag-cont)          '__ev-tag-cont)
           ((eq? f ev-and-pat-cont)      '__ev-and-pat-cont)
           ((eq? f ev-view-call-cont)    '__ev-view-call-cont)
           ((eq? f ev-view-match-cont)   '__ev-view-match-cont)
           ((eq? f ev-match-rest-cont)   '__ev-match-rest-cont)
           ((eq? f unwind-cont)          '__unwind-cont)
           ((eq? f replace-answer-cont)  '__replace-answer-cont)
           (else '__XXX-cont))))        ;TODO panic hard
    (object<- (make-cont-script name f) ;XXX script should be static
              (cdr (vector->list k))))) ;TODO keep Squeam code from ever accessing an unwrapped cont at (cadr k) via extract-datum


;; Primitive types

(define (get-script name)
  (script<- name (get-prim name) primitive-env))

(define (get-prim name)
  (env-lookup primitive-env name halt-cont))

(run-load "lib/runtime.scm")

(define miranda-trait (get-prim 'miranda-trait))

(define boolean-script (get-script 'claim-primitive))
(define number-script (get-script 'number-primitive))
(define nil-script    (get-script 'nil-primitive))
(define pair-script   (get-script 'cons-primitive))
(define symbol-script (get-script 'symbol-primitive))
(define char-script   (get-script 'char-primitive))
(define string-script (get-script 'string-primitive))
(define array-script  (get-script 'array-primitive))
(define box-script    (get-script 'box-primitive))
(define source-script (get-script 'source-primitive))
(define sink-script   (get-script 'sink-primitive))
(define term-script   (get-script 'term-primitive))
(define procedure-script (get-script 'procedure-primitive))
(define void-script   (get-script 'void-primitive))
(define script-script (get-script 'script-primitive))
(define ejector-script (get-script 'ejector-primitive))

(define the-map<- (get-prim 'map<-))


;; For tuning later.

(define (report-stats)
  'ok)
