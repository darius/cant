#!chezscheme
(library (player player)
(export run-load cant-interpret)
(import (chezscheme)
  (player util)
  (player macros)
  (player read)
  (player parse)
  (player env)
  (player setting)
  (player thing)
  (player elaborate)
  (player primitives)
  )

;(define (set-dbg! debug?)
;  (set! dbg (if debug? pp (lambda (x) #f))))

;(define (dbg x)
;  (pp x))
;  (report x)
; #f)


;; Argument tuples used to be lists, now they're terms.

(define (tuple? x)
  (and (term? x) (eq? (term-tag x) '~)))

(define (tuple<- . xs)
  (make-term '~ xs))

(define null-tuple
  (make-term '~ '()))

(define (list<-tuple tuple)
  ;;TODO drop insist if expensive -- should be redundant
  (insist (tuple? tuple) "Expected a tuple" tuple)
  (term-parts tuple))


;; Bootstrap prereqs

;; These integer tags k-foo correspond to procedures named cont-foo.
(define-enum
  k-halt 
  k-match-clause
  k-ev-trait-make
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
  k-disable-ejector
  k-call-unwind-thunk
  k-keep-unwinding
  k-replace-answer)

(define cont<- vector)

(define halt-cont (cont<- k-halt))

(define (get-prim name)
  (really-global-lookup name))


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

(define (global-lookup v k)
  (let ((value (really-global-lookup v)))
    (if (eq? value missing)
        (signal k "Unbound variable" v)
        (answer k value))))

(define (env-lookup r v k)
  (define (succeed pair) (answer k (cdr pair)))
  (cond ((assq v r) => succeed)
        (else (global-lookup v k))))

(define (env-extend-promises r vs)
  (let consing ((vs vs) (r r))
    (if (null? vs)
        r
        (consing (cdr vs) (cons (cons (car vs) uninitialized) r)))))

(define (env-resolve! r v value k)
  (cond ((assq v r) => (lambda (pair)
                         (if (not (eq? (cdr pair) uninitialized))
                             (signal k "Multiple definition" v)
                             (begin (set-cdr! pair value)
                                    (answer k #t)))))
        ((null? r)
         (really-global-define! v value)
         (answer k #t))
        (else (signal k "Tried to bind in a non-environment" r v))))


;; Objects, calling, and answering

(define-record-type cps-script (fields name procedure))

(define (cps-prim<- datum name procedure)
  (object<- (make-cps-script name procedure) datum))

;; Helper for coding a cps-prim. Check the arguments' arity and unpack them.
(define (cps-unpack arguments k arity name ok-fn)
  (if (= (length arguments) arity)
      (apply ok-fn arguments)
      (signal k "Wrong #arguments to:" name "expects:" arity "got:" arguments)))
  
;; A continuation is just a vector, whose zeroth element is a small
;; integer denoting which continuation procedure. Previously this
;; element was the Scheme procedure itself, which had to never be
;; exposed directly to Cant code, only wrapped as an object with an
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
             (if (tuple? message)
                 ((cps-script-procedure script) datum (list<-tuple message) k)
                 (delegate (get-prim 'cps-primitive)
                           object message k)))
            (else
             (error 'call "Not a script" script datum)))))
   ((procedure? object)
    (if (tuple? message)
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
                      (apply object (list<-tuple message)))))))
        (run-script object procedure-script object message k)))
   (else
    (let ((script (extract-script object)))
      (run-script object script object message k)))))

(define reply-prim 
  (cps-prim<- #f '__reply
              (lambda (_datum arguments _k)
                (cps-unpack arguments _k 2 '__reply
                            (lambda (alleged-k result)
                              (unless (seems-to-be-a-raw-repr? alleged-k methods/cont)
                                (error 'reply "Not a cont" alleged-k))
                              ;; N.B. ignore _k from above
                              (answer alleged-k result))))))

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
   ((mapi? object)        map-script)
   ((eq? object (void))   void-script)
   ((eof-object? object)  eof-script)
   ((script? object)      script-script)
   ((procedure? object)   procedure-script)
   ((setting? object)     setting-script)
   ((object? object)      (object-script object))
   (else (error 'call "Non-object" object))))

(define (extract-datum object)
  (cond
   ((object? object)      (object-datum object))
   ;; XXX: script too?
   (else                  object)))

(define (handle-error k evil)
  (let ((handler (get-prim '__handle-error)) ;TODO do this just once
        (message (tuple<- k evil)))
    (when (eq? handler missing)
      (error 'handle-error "Error before the handler wrapper even got defined"
             evil))
    (call handler message halt-cont)))

(define error-prim
  (cps-prim<- #f 'error
              (lambda (datum arguments k)
                (handle-error k arguments))))

(define panic-prim
  (cps-prim<- #f 'panic
              (lambda (datum arguments k)
                (let ((message-for-chez ;Chez Scheme is picky about arguments to (error).
                       (if (and (pair? arguments) (string? (car arguments)))
                           arguments
                           (cons "Error" arguments))))
                  (apply error 'panic message-for-chez)))))

(define (delegate trait object message k)
;  (dbg `(delegate))
  (let ((handler (cond ((object? trait) trait)
                       ((not trait) miranda-trait)
                       (else (error 'delegating "Unknown trait type" trait)))))
    (call handler (tuple<- object message) k)))

(define (signal k . evil)
  (handle-error k evil))


;; Ejectors

;; An ejector has two facets:
;;   An facade of 'ejector' type, exposed to ordinary Cant code.
;;     - This has a datum, a box holding either #f or a reference to the
;;       other facet. This tells either that this ejector is disabled (#f),
;;       or where to unwind to, when ejecting.
;;   A raw continuation vector of type k-disable-ejector, with one data slot:
;;     - The same box described above.
;;     The action on either replying or unwinding is to set the box to #f.
;; (This could've had a more straightforward representation, but that
;; would've held on to more garbage after the disabling.)

;; The new continuation vector becomes the sequel to the receiver of
;; the new ejector. Its parent sequel, of course, is the sequel to the
;; with-ejector call.

;; The related type of raw continuation vector, k-call-unwind-thunk, has one
;; data slot:
;;   - an unwind thunk
;; On either replying or unwinding, it calls the unwind thunk before
;; continuing back. Any reply from the thunk will be dropped, but if
;; it errors or ejects then that action takes precedence.

;; Call the receiver with a new, enabled ejector.
(define with-ejector-prim
  (cps-prim<-
   #f 'with-ejector
   (lambda (datum arguments k)
     (cps-unpack arguments k 1 'with-ejector
                 (lambda (receiver)
                   (let* ((ejector-box (box '*))
                          (ejector-k (cont<- k-disable-ejector k ejector-box))
                          (ejector (object<- ejector-script ejector-box)))
                     (set-box! ejector-box ejector-k)
                     (call receiver (tuple<- ejector) ejector-k)))))))

;; Call thunk; then on either reply or unwind, call unwind-thunk
;; before continuing back. Any reply from unwind-thunk will be
;; dropped, but if it errors or ejects then that action takes
;; precedence.
;; TODO untested
(define ejector-protect-prim                 ;TODO rename
  (cps-prim<-
   #f 'ejector-protect
   (lambda (datum arguments k)
     (cps-unpack arguments k 2 'ejector-protect
                 (lambda (thunk unwind-thunk)
                   (call thunk null-tuple
                         (cont<- k-call-unwind-thunk k unwind-thunk)))))))

;; The method (ejector .eject result)
(define eject-prim
  (cps-prim<-
   #f '__eject
   (lambda (datum arguments k)
     (cps-unpack arguments k 2 '__eject
                 (lambda (ejector result)
                   (unless (and (object? ejector)
                                (eq? (object-script ejector) ejector-script))
                     (error 'bug "Not an ejector" ejector))
                   (let* ((ejector-box (object-datum ejector))
                          (state (unbox ejector-box)))
                     (cond ((not state)
                            (signal k "Tried to eject to a disabled ejector"
                                    ejector))
                           (else
                             (insist (seems-to-be-a-raw-repr? state methods/cont)
                                     "Ejector cont is a cont" state)
                             (insist (= k-disable-ejector (vector-ref state 0))
                                     "Ejector cont is legit" state)
                             ;; TODO Is it wise to wait to disable
                             ;; this ejector until the unwinding
                             ;; reaches it? Or better disable it right
                             ;; now? I'm choosing the former, so
                             ;; unwind-thunks along the way can also
                             ;; run this ejector, since I don't see
                             ;; why to deny them. But I dunno, man.
                             (ejector-unwinding k state result)))))))))

(define (cont-disable-ejector result k0)
  (unpack k0 (k state)
    (set-box! state #f)
    (answer k result)))

(define (ejector-unwinding k ejector-k result)
  (let ((k-action (vector-ref k 0))
        (parent-k (vector-ref k 1)))
    (cond ((= k-action k-disable-ejector)
           (let ((state (vector-ref k 2)))
             (set-box! state #f))
           (if (eq? k ejector-k)
               (answer k result)
               (ejector-unwinding parent-k ejector-k result)))
          ((= k-action k-call-unwind-thunk)
           (let ((unwind-thunk (vector-ref k 2)))
             (call unwind-thunk '()
                   (cont<- k-keep-unwinding parent-k ejector-k result))))
          (else
           (ejector-unwinding parent-k ejector-k result)))))

(define (cont-keep-unwinding value-to-ignore k0)
  (unpack k0 (k ejector-k result)
    (ejector-unwinding k ejector-k result)))

;; The procedure for an ordinary reply to a k-call-unwind-thunk record.
(define (cont-call-unwind-thunk result k0)
  (unpack k0 (k unwind-thunk)
    (call unwind-thunk null-tuple
          (cont<- k-replace-answer k result))))

(define (cont-replace-answer value-to-ignore k0)
  (unpack k0 (k result)
    (answer k result)))


;; A small-step interpreter

(define setting-lookup-prim
  (cps-prim<- #f '__setting-lookup
              (lambda (datum arguments k)
                (cps-unpack arguments k 2 '__setting-lookup
                            (lambda (r variable)
                              (env-lookup r variable k))))))

(define setting-resolve!-prim
  (cps-prim<- #f '__setting-resolve!
              (lambda (datum arguments k)
                (cps-unpack arguments k 3 '__setting-resolve!
                            (lambda (r variable value)
                              (env-resolve! r variable value k))))))

(define evaluate-prim
  (cps-prim<- #f 'evaluate
              (lambda (datum arguments k)
                (cps-unpack arguments k 2 '__evaluate
                            (lambda (e setting)
                              (evaluate-exp e setting k))))))

(define (evaluate e setting)
;;  (report `(evaluate ,e))
  (evaluate-exp e setting halt-cont))

(define (evaluate-exp e setting k)
  ;; An inherently incomplete sanity check, not a real security barrier:
  (unless (seems-to-be-a-raw-repr? e methods/ev-exp)
    (error 'evaluate-exp "You need to parse it first" e))
  (unless (setting? setting)
    (error 'evaluate-exp "Wrong type: expected a setting" setting))
  (let* ((elaboration (elaborate e setting))
         (elaborated-e (car elaboration))
         (maybe-extended-setting (cadr elaboration)))
    (ev-exp elaborated-e (setting-a-list maybe-extended-setting) k)))

(define (ev-exp e r k)
;;  (dbg `(ev-exp ,(pack-tag e))) ; ,e))
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
                   (cont<- k-ev-trait-make k r name clauses)))))
   (lambda (e r k)                          ;e-do
     (unpack e (e1 e2)
       (ev-exp e1 r (cont<- k-ev-do-rest k r e2))))
   (lambda (e r k)                          ;e-let
     (unpack e (p e1)
       (ev-exp e1 r (cont<- k-ev-let-match k r p))))
   (lambda (e r k)                          ;e-call
     (unpack e (e1 e2)
       (ev-exp e1 r (cont<- k-ev-arg k r e2))))
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
       (answer k (cant=? subject value))))
   (lambda (subject p r k)              ;p-any
     (answer k #t))
   (lambda (subject p r k)              ;p-variable
     (unpack p (name)
       (env-resolve! r name subject k)))
   (lambda (subject p r k)              ;p-term
     (unpack p (tag args)
       (if (not (and (term? subject)
                     (cant=? (term-tag subject) tag)))
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

(define (cont-halt value k0) value)

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

(define (cont-match-clause matched? k0)
;     (dbg `(cont-match-clause))
  (unpack k0 (k pat-r body rest-clauses object script datum message)
               ;; TODO don't unpack it all till needed
  ;; body is now a list (body-vars body-exp)
    (if matched?
        (ev-exp (cadr body) (env-extend-promises pat-r (car body)) k)
        (matching rest-clauses object script datum message k))))

(define (cont-ev-trait-make trait-val k0)
;     (dbg `(ev-make-cont))
  (unpack k0 (k r name clauses)
    (answer k (object<- (script<- name trait-val clauses)
                        r))))

(define (cont-ev-do-rest _ k0)
;     (dbg `(cont-ev-do-rest))
  (unpack k0 (k r e2)
    (ev-exp e2 r k)))

(define (cont-ev-let-match val k0)
;     (dbg `(cont-ev-let-match))
  (unpack k0 (k r p)
    (ev-pat val p r
            (cont<- k-ev-let-check k val))))

(define (cont-ev-let-check matched? k0)
;     (dbg `(cont-ev-let-check))
  (unpack k0 (k val)
    (if matched?
        (answer k val)
        (signal k "Match failure" val))))

(define (cont-ev-arg receiver k0)
;     (dbg `(cont-ev-arg))
  (unpack k0 (k r e2)
    (ev-exp e2 r
            (cont<- k-ev-call k receiver))))

(define (cont-ev-call message k0)
;     (dbg `(cont-ev-call ,receiver ,message))
  (unpack k0 (k receiver)
    (call receiver message k)))

(define (cont-ev-rest-args val k0)
;     (dbg `(cont-ev-rest-args))
  (unpack k0 (k r es vals)
    (ev-args es r (cons val vals) k)))

(define (cont-ev-tag vals k0)
;     (dbg `(cont-ev-tag))
  (unpack k0 (k tag)
    (answer k (make-term tag vals))))

(define (cont-ev-and-pat matched? k0)
;     (dbg `(cont-ev-and-pat))
  (unpack k0 (k r subject p2)
    (if matched?
        (ev-pat subject p2 r k)
        (answer k #f))))

(define (cont-ev-view-call convert k0)
;     (dbg `(cont-ev-view-call))
  (unpack k0 (k r subject p)
    (call convert (tuple<- subject)
          (cont<- k-ev-view-match k r p))))

(define (cont-ev-view-match new-subject k0)
;     (dbg `(cont-ev-view-match))
  (unpack k0 (k r p)
    (ev-pat new-subject p r k)))

(define (cont-ev-match-rest matched? k0)
;     (dbg `(cont-ev-match-rest))
  (unpack k0 (k r subjects ps)
    (if matched?
        (ev-match-all subjects ps r k)
        (answer k #f))))

(define methods/cont
  (vector
   cont-halt
   cont-match-clause
   cont-ev-trait-make
   cont-ev-do-rest
   cont-ev-let-match
   cont-ev-let-check
   cont-ev-arg
   cont-ev-call
   cont-ev-rest-args
   cont-ev-tag
   cont-ev-and-pat
   cont-ev-view-call
   cont-ev-view-match
   cont-ev-match-rest
   cont-disable-ejector
   cont-call-unwind-thunk
   cont-keep-unwinding
   cont-replace-answer))


;; Interpreter top level

(define (run-load filename)
  (let ((forms (snarf filename cant-read)))
    (cant-interpret `(do ,@forms))))

;; TODO add setting & optional context
(define (cant-interpret e)
  (evaluate (parse-exp e) (make-setting repl-env)))

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
(define map-script       '*forward-ref*)
(define setting-script   '*forward-ref*)

(define (get-script name)
  (script<- name (get-prim name) primitive-env))

(define mask32 (- (expt 2 32) 1))

(for-each (lambda (pair)
            (let ((key (car pair)) (value (cadr pair)))
              (global-init! key value)))
  `((__as-link ,as-link)
    (= ,cant=?)
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
    (assoc ,assoc)                      ;XXX doesn't use cant=?
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
    (__eject ,eject-prim)
    (ejector-protect ,ejector-protect-prim)
    (__reply ,reply-prim)

    ;; These will get high-level definitions later TODO
    (void ,(void))
    (/ ,/)
    (expt ,expt)
    (abs ,abs)
    (gcd ,gcd)
    (__array<-list ,list->vector)
    (read ,cant-read)
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
    (setting? ,setting?)
    (immutable-map? ,mapi?)             ;TODO sheesh the name
    (map<-items ,prim-mapi<-items)

    ;; Primitives only -- TODO seclude in their own env:
    (__setting<- ,make-setting)
    (__setting-a-list ,setting-a-list)
    (__setting-lookup ,setting-lookup-prim)
    (__setting-extend-promises ,env-extend-promises)
    (__setting-resolve! ,setting-resolve!-prim)
    (__setting-binds? ,setting-binds?)
    (__mapi-items ,mapi-items)
    (__mapi-get ,prim-mapi-get)
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
    (__term-parts ,term-parts)
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

(run-load "abcs/10-runtime.cant")

(set! miranda-trait  (get-prim 'miranda-trait))

(set! boolean-script (get-script 'claim-primitive))
(set! number-script  (get-script 'number-primitive))
(set! nil-script     (get-script 'nil-primitive))
(set! link-script    (get-script 'link-primitive))
(set! symbol-script  (get-script 'symbol-primitive))
(set! char-script    (get-script 'char-primitive))
(set! string-script  (get-script 'string-primitive))
(set! array-script   (get-script 'array-primitive))
(set! box-script     (get-script 'box-primitive))
(set! source-script  (get-script 'source-primitive))
(set! sink-script    (get-script 'sink-primitive))
(set! term-script    (get-script 'term-primitive))
(set! procedure-script (get-script 'procedure-primitive))
(set! void-script    (get-script 'void-primitive))
(set! eof-script     (get-script 'eof-primitive))
(set! script-script  (get-script 'script-primitive))
(set! ejector-script (get-script 'ejector-primitive))
(set! map-script     (get-script 'map-primitive))
(set! setting-script (get-script 'setting-primitive))


;; For tuning later.

;(define (report-stats)
;  'ok)

)
