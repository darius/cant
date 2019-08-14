#!chezscheme
(library (player player)
(export run-load squeam-interpret)
(import (chezscheme)
  (player util)
  (player macros)
  (player read)
  (player parse)
  (player env)
  (player elaborate)
  (player primitives)
  )

;; (define (set-dbg! debug?)
;;   (set! dbg (if debug? pp (lambda (x) #f))))

;; (define (dbg x)
;; ;  (pp x))
;;  #f)


;; Bootstrap prereqs

(define-record-type object (fields script datum))   ; Nonprimitive objects, that is.
(define object<- make-object)

(define-record-type script (fields name trait clauses))
(define script<- make-script)

(define-enum
  k-halt 
  k-match-clause
  k-ev-trait-make-cont
  k-ev-do-rest
  k-ev-let-match
  k-ev-let-check
  k-ev-arg
  k-ev-call
  k-ev-rest-args
  k-ev-tag
  k-ev-and-pat
  k-ev-view-call
  k-ev-view-match
  k-ev-match-rest
  k-unwind
  k-keep-unwinding
  k-replace-answer)

(define cont<- vector)

(define halt-cont (cont<- k-halt))

(define (get-prim name)
  (really-global-lookup name))

(define mask32 (- (expt 2 32) 1))


;; Primitive depiction

(define (depict x)
  (cond ((object? x)
         (string-append "#<"
                        (let ((script (object-script x)))
                          (cond ((script? script)
                                 (script-name script))
                                (else "XXX-WTF")))
                        ">"))
        (else
         ;; This only gets called from miranda-trait, so x already had
         ;; a chance to depict itself in its preferred way.
         (call-with-string-output-port
          (lambda (p) (put-datum p x))))))


;; Environments

(define (global-define! v value k)
  (really-global-define! v value)
  (answer k #t))

(define (global-lookup v k)
  (let ((value (really-global-lookup v)))
    (if (eq? value missing)
        (signal k "Unbound variable" v)
        (answer k value))))

(define (env-defined? r v)
  (define (succeed pair) #t)  ;XXX or (not (eq? (cadr pair) uninitialized)))
  (cond ((assq v r) => succeed)
        (else (global-defined? v))))

(define (env-lookup r v k)
  (define (succeed pair) (answer k (cadr pair)))
  (cond ((assq v r) => succeed)
        (else (global-lookup v k))))

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
         (global-define! v value k))
        (else (signal k "Tried to bind in a non-environment" r v))))

(define uninitialized
  (object<- (script<- '<uninitialized> #f '()) '*uninitialized*))

(define (env-variables r)
  (map car r))


;; Objects, calling, and answering

(define-record-type cps-script (fields name procedure))

(define (cps-prim<- datum name procedure)
  (object<- (make-cps-script name procedure) datum))

;; A continuation is just a vector, whose zeroth element is a small
;; integer denoting which continuation procedure. Previously this
;; element was the Scheme procedure itself, which had to never be
;; exposed directly to Squeam code, only wrapped as an object with an
;; appropriate wrapper script. Let's hope the new extra indirection
;; isn't too costly.
(define (answer k value)
;  (dbg `(answer ,value ,k))
;  (dbg `(answer))
  ((vector-ref methods/cont (vector-ref k 0)) value k))

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

(define reply-prim 
  (cps-prim<- #f '__reply
              (lambda (_datum message _k)
                (unless (= 2 (length message))
                  (error 'reply "Wrong #arguments" message))
                (let ((alleged-k (car message))
                      (result (cadr message)))
                  (unless (seems-to-be-a-raw-repr? alleged-k methods/cont)
                    (error 'reply "Not a cont" alleged-k))
                  ;; N.B. ignore _k from above
                  (answer alleged-k result)))))

(define (seems-to-be-a-raw-repr? x methods)
  (and (vector? x)
       (< 0 (vector-length x))
       (integer? (vector-ref x 0))
       (<= 0 (vector-ref x 0))
       (< (vector-ref x 0) (vector-length methods))))

(define (extract-script object)
  (cond
   ((number? object)      number-script)
   ((vector? object)      array-script)
   ((pair? object)        link-script)
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
   ((eof-object? object)  eof-script)
   ((script? object)      script-script)
   ((procedure? object)   procedure-script)
   ((object? object)      (object-script object))
   (else (error 'call "Non-object" object))))

(define (extract-datum object)
  (cond
   ((object? object)      (object-datum object))
   ;; XXX: script too?
   (else                  object)))

(define (handle-error k evil)
  (let ((handler (get-prim '__handle-error)) ;TODO do this just once
        (message (list k evil)))
    (when (eq? handler missing)
      (error 'handle-error "Error before the handler wrapper even got defined"
             evil))
    (call handler message halt-cont)))

(define error-prim
  (cps-prim<- #f 'error
              (lambda (datum message k)
                (handle-error k message))))

(define (panic message)
  (let ((message-for-chez ;Chez Scheme is picky about arguments to (error).
         (if (and (pair? message) (string? (car message)))
             message
             (cons "Error" message))))
    (apply error 'panic message-for-chez)))

(define panic-prim
  (cps-prim<- #f 'panic
              (lambda (datum message k)
                (panic message))))

(define (delegate trait object message k)
;  (dbg `(delegate))
  (let ((handler (cond ((object? trait) trait)
                       ((not trait) miranda-trait)
                       (else (error 'delegating "Unknown trait type" trait)))))
    (call handler (list object message) k)))

(define (signal k . evil)
  (handle-error k evil))


;; Ejectors

(define (ejector<- ejector-k)
  (object<- ejector-script ejector-k))

(define (unwind-cont value k0)
  (unpack k0 (k unwind-action)
    (unwind-action k0 (cont<- k-replace-answer k value))))

(define (replace-answer-cont value-to-ignore k0)
  (unpack k0 (k value)
    (answer k value)))

;; k0 is like #(unwind-cont parent-k unwind-ejector enabled?)
(define (unwind-ejector k0 k)
  (vector-set! k0 3 #f)
  (answer k (void)))

(define ejector-eject-prim
  (cps-prim<- #f '__eject
              (lambda (datum message k)
                (insist (= (length message) 2) "ejector-eject wrong #arguments"
                        (- (length message) 1))
                (let ((ejector (car message))
                      (value (cadr message)))
                  (if (and (object? ejector)
                           (eq? (object-script ejector) ejector-script))
                      (let ((ejector-k (object-datum ejector)))
                        (insist (seems-to-be-a-raw-repr? ejector-k methods/cont)
                                "Ejector cont is a cont" ejector-k)
                        (insist (= k-unwind (vector-ref ejector-k 0))
                                "Ejector cont is an unwind cont" ejector-k)
                        (if (vector-ref ejector-k 3) ;still enabled?
                            (ejector-unwinding k ejector-k value)
                            (signal k "Tried to eject to a disabled ejector"
                                    ejector)))
                      (signal k "Not an ejector" ejector))))))

(define (ejector-unwinding k ejector-k value)
  (if (eq? k ejector-k)
      (answer ejector-k value)
      (let ((k-action (vector-ref k 0))
            (parent-k (vector-ref k 1)))
        (if (= k-action k-unwind)
            (let ((unwind-action (vector-ref k 2)))
              (unwind-action k (cont<- k-keep-unwinding parent-k ejector-k value)))
            (ejector-unwinding parent-k ejector-k value)))))

(define (keep-unwinding value-to-ignore k0)
  (unpack k0 (k ejector-k value)
    (ejector-unwinding k ejector-k value)))

(define (do-ejector-protect datum message k)
  ;;XXX check arity
  (let ((thunk (car message))
        (unwind-thunk (cadr message)))
    (call thunk '() (cont<- k-unwind k call-unwinder unwind-thunk))))

(define (call-unwinder k0 k)
  (let ((unwind-thunk (vector-ref k0 3)))
    (call unwind-thunk '() k)))

(define ejector-protect-prim                 ;TODO rename
  (cps-prim<- #f 'ejector-protect do-ejector-protect))

(define with-ejector-prim
  (cps-prim<- #f 'with-ejector
              (lambda (datum message k)
                ;;XXX check arity
                (let* ((ejector-k
                        (cont<- k-unwind k unwind-ejector #t))
                       (ejector (ejector<- ejector-k)))
                  (call (car message) (list ejector) ejector-k)))))


;; A small-step interpreter

(define evaluate-prim
  (cps-prim<- #f 'evaluate
              (lambda (datum message k)
                (if (= (length message) 2)
                    (evaluate-exp (car message) (cadr message) k)
                    (signal k "Wrong number of arguments -- evaluate" message)))))

(define (evaluate e r)
;  (report `(evaluate ,e))
  (evaluate-exp e r halt-cont))

(define (evaluate-exp e r k)
  ;; An inherently incomplete sanity check, not a real security barrier:
  (unless (seems-to-be-a-raw-repr? e methods/ev-exp)
    (error 'evaluate-exp "You need to parse it first" e))

  ;; XXX Here we just jam the vars-defined of e together
  ;; with r's variables. This is adequate for warning about
  ;; unbound variables in e, which is all we're currently
  ;; using this scope for, but it won't be adequate when we
  ;; compile to lexical addresses, etc.:
  (let* ((vars (append (exp-vars-defined e)
                       (env-variables r)))
         (elaborated (elaborate-e e (outer-scope<- vars))))
    (ev-exp elaborated r k)))

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
                (cont<- k-ev-tag k tag))))
   (lambda (e r k)                          ;e-list
     (unpack e (es)
       (ev-args es r '() k)))
   (lambda (e r k)                          ;e-make
     (unpack e (name trait clauses)
       (if (eq? trait none-exp) ; Just fast-path tuning; this IF is not logically necessary.
           (answer k (object<- (script<- name #f clauses) ;TODO cache the script in the parsed 'make' exp
                               r))
           (ev-exp trait r
                   (cont<- k-ev-trait-make-cont k r name clauses)))))
   (lambda (e r k)                          ;e-do
     (unpack e (e1 e2)
       (ev-exp e1 r (cont<- k-ev-do-rest k r e2))))
   (lambda (e r k)                          ;e-let
     (unpack e (p e1)
       (ev-exp e1 r (cont<- k-ev-let-match k r p))))
   (lambda (e r k)                          ;e-call
     (unpack e (e1 e2)
       (ev-exp e1 r (cont<- k-ev-arg k r e2))))
   (lambda (e r k)                          ;e-global
     (unpack e (var)
       (global-lookup var k)))
   ))

(define (ev-args es r vals k)
  (if (null? es)
      (answer k (reverse vals))
      (ev-exp (car es) r
              (cont<- k-ev-rest-args k r (cdr es) vals))))

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
               (cont<- k-ev-and-pat k r subject p2))))
   (lambda (subject p r k)              ;p-view
     (unpack p (e p1)
       (ev-exp e r
               (cont<- k-ev-view-call k r subject p1))))))

(define (ev-match-all subjects ps r k)
  (cond ((null? ps)
         (answer k (null? subjects)))
        ((null? subjects)
         (answer k #f))
        (else
         (ev-pat (car subjects) (car ps) r
                 (cont<- k-ev-match-rest k r (cdr subjects) (cdr ps))))))

(define (halt-cont-fn value k0) value)

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
               (cont<- k-match-clause k pat-r body rest-clauses object script datum message))))))  ;XXX geeeez

(define (match-clause-cont matched? k0)
;     (dbg `(match-clause-cont))
  (unpack k0 (k pat-r body rest-clauses object script datum message)
               ;; TODO don't unpack it all till needed
  ;; body is now a list (body-vars body-exp)
    (if matched?
        (ev-exp (cadr body) (env-extend-promises pat-r (car body)) k)
        (matching rest-clauses object script datum message k))))

(define (ev-trait-make-cont trait-val k0)
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
            (cont<- k-ev-let-check k val))))

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
            (cont<- k-ev-call k receiver))))

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
          (cont<- k-ev-view-match k r p))))

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

(define methods/cont
  (vector
   halt-cont-fn
   match-clause-cont
   ev-trait-make-cont
   ev-do-rest-cont
   ev-let-match-cont
   ev-let-check-cont
   ev-arg-cont
   ev-call-cont
   ev-rest-args-cont
   ev-tag-cont
   ev-and-pat-cont
   ev-view-call-cont
   ev-view-match-cont
   ev-match-rest-cont
   unwind-cont
   keep-unwinding
   replace-answer-cont))


;; Interpreter top level

(define (run-load filename)
  (let ((forms (snarf filename squeam-read)))
    (squeam-interpret `(do ,@forms))))

;; TODO add optional context
(define (squeam-interpret e)
  (evaluate (parse-exp e) repl-env))

(define (parse-exp e . opt-context)
  (parse-e e (optional-context 'parse-exp opt-context)))

(define (parse-pat p . opt-context)
  (parse-p p (optional-context 'parse-pat opt-context)))


;; Install the primitives, load the scripts and runtime env

(define miranda-trait    '*forward-ref*)

(define boolean-script   '*forward-ref*)
(define number-script    '*forward-ref*)
(define nil-script       '*forward-ref*)
(define link-script      '*forward-ref*)
(define symbol-script    '*forward-ref*)
(define char-script      '*forward-ref*)
(define string-script    '*forward-ref*)
(define array-script     '*forward-ref*)
(define box-script       '*forward-ref*)
(define source-script    '*forward-ref*)
(define sink-script      '*forward-ref*)
(define term-script      '*forward-ref*)
(define procedure-script '*forward-ref*)
(define void-script      '*forward-ref*)
(define eof-script       '*forward-ref*)
(define script-script    '*forward-ref*)
(define ejector-script   '*forward-ref*)

(define (get-script name)
  (script<- name (get-prim name) primitive-env))

(for-each (lambda (pair)
            (let ((key (car pair)) (value (cadr pair)))
              (global-init! key value)))
  `((__as-link ,as-link)
    (= ,squeam=?)
    (out ,(current-output-port))
    (stdin ,(current-input-port))       ;XXX inconsistent

    (link ,list*) ;;TODO insist that last argument = nil or pair? TODO is this n-arg form useful? 
    (link? ,pair?)
    (null? ,null?)
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
    (source? ,input-port?) ;TODO these only know about primitive sources/sinks
    (sink? ,output-port?)
    (eof? ,eof-object?)
    (box<- ,box<-)
    (symbol<- ,string->symbol)
    (term<- ,make-term)       ;TODO check that arguments arg is a list
    (char<- ,integer->char)
    (__string<-list ,list->string)
    (array<-count ,make-vector)
    (inexact<-exact ,exact->inexact)  ;XXX rename or something
    (exact<-inexact ,inexact->exact)  ;XXX rename or something
    (floor ,floor)
    (not ,not)
    (assoc ,assoc)
    (sqrt ,sqrt)
    (panic ,panic-prim)
    (error ,error-prim)
    (__evaluate ,evaluate-prim)
    (open-input-file ,open-input-file)  ;XXX rename open-file-source
    (open-output-file ,open-output-file) ; open-file-sink
    (open-binary-file-source ,open-file-input-port) ; This actually has more options in Chez than just binary
    (open-binary-file-sink ,open-file-output-port)  ; but let's just hack it in for now. 
    (__get-u8 ,get-u8)
    (__put-u8 ,put-u8)
;;    (__set-dbg! ,set-dbg!)
    (with-ejector ,with-ejector-prim)
    (__eject ,ejector-eject-prim)
    (ejector-protect ,ejector-protect-prim)
    (__reply ,reply-prim)

    ;; These will get high-level definitions later TODO
    (void ,(void))
    (/ ,/)
    (expt ,expt)
    (abs ,abs)
    (gcd ,gcd)
    (__array<-list ,list->vector)
    (read ,squeam-read)
    (__parse-exp ,parse-exp)
    (__parse-pat ,parse-pat)
    (system ,system)
    ;; Should use string ports instead:
    (number<-string ,string->number)
    (string<-number ,number->string)
    (list<-string ,string->list)
    (string-source<- ,open-input-string)
    (string-sink<- ,open-output-string)
    (__get-output-string ,get-output-string)
    (self-evaluating? ,self-evaluating?)
    (maybe-macroexpand-expr ,maybe-macroexpand-expr)
    (maybe-macroexpand-patt ,maybe-macroexpand-patt)
    (open-subprocess ,process)
    (global-defined? ,global-defined?)
    (extract-script ,extract-script)
    (extract-datum ,extract-datum)
    (__halp-log ,prim-halp-log)
    (nano-now ,prim-nano-now)
    (nanosleep ,prim-nanosleep)

    ;; Primitives only -- TODO seclude in their own env:
    (__place ,hashmap-place)
    (__char-compare ,char-compare)
    (__number-compare ,number-compare)
    (__+ ,+)
    (__- ,-)
    (__* ,*)
    (__quotient ,quotient)
    (__remainder ,remainder)
    (__modulo ,modulo)
    (__*/mod ,prim-*/mod)
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
    (__symbol->string ,symbol->string)
    (__string-append ,string-append)
    (__string-compare ,string-compare)
    (__string-length ,string-length)
    (__string-maps? ,prim-string-maps?)
    (__string-ref ,string-ref)
    (__substring ,prim-substring)
    (__vector-append ,vector-append)
    (__vector-copy ,vector-copy)
    (__vector-length ,vector-length)
    (__vector-maps? ,prim-vector-maps?)
    (__vector-ref ,vector-ref)
    (__vector-set! ,vector-set!)
    (__vector->list ,vector->list)
    (__subvector ,subvector)            ;XXX redefine this to result in a view instead of a copy? ditto for strings
    (__vector-move! ,vector-move!)
    (__char->integer ,char->integer)
    (__char-digit? ,char-numeric?)
    (__char-letter? ,char-alphabetic?)
    (__char-whitespace? ,char-whitespace?)
    (__char-lowercase? ,char-lower-case?)
    (__char-uppercase? ,char-upper-case?)
    (__char-lowercase ,char-downcase)
    (__char-uppercase ,char-upcase)
    (__box-value ,unbox)
    (__box-value-set! ,set-box!)
    (__term-tag ,term-tag)
    (__term-arguments ,term-parts)
    (__close-port ,close-port)
    (__read-char ,read-char)
    (__char-ready? ,char-ready?)
    (__read-all ,prim-read-all)
    (__write-char ,write-char)
    (__display ,prim-display)
    (__depict ,depict)

    (__u+ ,(lambda (a b) (logand mask32 (+ a b)))) ;XXX revisit these definitions
    (__s+ ,(lambda (a b) (logand mask32 (+ a b)))) ;XXX I forget what distinction I meant to make
    (__s* ,(lambda (a b) (logand mask32 (* a b))))
    (__u- ,(lambda (a b) (logand mask32 (- a b))))
    (__u/ ,(lambda (a b) (logand mask32 (fx/ a b))))
    (__u<< ,(lambda (a b) (logand mask32 (ash a b))))
    (__u>> ,(lambda (a b) (logand mask32 (ash a (- b)))))

    (__cps-primitive-name ,(lambda (x)
                             (cps-script-name (object-script x))))
    (__script-name ,script-name)
    (__script-trait ,script-trait)
    (__script-clauses ,script-clauses)
    (os-exit ,exit)
    ))

(run-load "abcs/20-runtime.scm")

(set! miranda-trait (get-prim 'miranda-trait))

(set! boolean-script (get-script 'claim-primitive))
(set! number-script (get-script 'number-primitive))
(set! nil-script    (get-script 'nil-primitive))
(set! link-script   (get-script 'link-primitive))
(set! symbol-script (get-script 'symbol-primitive))
(set! char-script   (get-script 'char-primitive))
(set! string-script (get-script 'string-primitive))
(set! array-script  (get-script 'array-primitive))
(set! box-script    (get-script 'box-primitive))
(set! source-script (get-script 'source-primitive))
(set! sink-script   (get-script 'sink-primitive))
(set! term-script   (get-script 'term-primitive))
(set! procedure-script (get-script 'procedure-primitive))
(set! void-script   (get-script 'void-primitive))
(set! eof-script    (get-script 'eof-primitive))
(set! script-script (get-script 'script-primitive))
(set! ejector-script (get-script 'ejector-primitive))


;; For tuning later.

;(define (report-stats)
;  'ok)

)
