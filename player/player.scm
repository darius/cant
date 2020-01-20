;; Interpreter of ASTs, in defunctionalized continuation-passing style.

#!chezscheme
(library (player player)
(export evaluate)
(import (chezscheme)
  (player util)
  (player macros)
  (player equality)
  (player read)
  (player ast)
  (player parse)
  (player setting)
  (player thing)
  (player elaborate)
  (player primordia)
  (player nonmeta-primitives)
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


;; Objects, calling, and answering

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

(define cps-script-trait (script-trait script/cps))

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
                 (delegate cps-script-trait object message k)))
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
        (run-script object script/procedure primordial-setting message k)))
   (else
    (let ((script (extract-script object)))
      (run-script object script primordial-setting message k)))))

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

(define (handle-error k evil)
  (let ((handler (unbox raw-signal-handler-box)))
    (unless handler
      (error 'handle-error "Error before the handler wrapper even got defined"
             evil))
    (call handler (tuple<- k evil) halt-cont)))

(define raw-signal-handler-box (box #f))

(define error-prim
  (cps-prim<- #f 'error
              (lambda (datum arguments k)
                (handle-error k arguments))))

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
;;   A facade of 'ejector' type, exposed to ordinary Cant code.
;;     - This has a datum, a box holding either #f or a reference to the
;;       other facet. This tells either that this ejector is disabled (#f),
;;       or where to unwind to, when ejecting.
;;   A raw continuation vector of type k-disable-ejector, with one data slot:
;;     - The same box described above.
;; The action on either replying or unwinding is to set the box to #f.
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
                          (ejector (object<- script/ejector ejector-box)))
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
                                (eq? (object-script ejector) script/ejector))
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
  (unless (or (not setting)  ;XXX
              (setting? setting))
    (error 'evaluate-exp "Wrong type: expected a setting" setting))
  (let* ((maybe-extended-setting (elaborate-setting e setting))
         (elaborated-e (elaborate e maybe-extended-setting)))
    (ev-exp elaborated-e maybe-extended-setting k)))

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
     (unpack e (depth offset)
       (let ((value (if depth
                        (setting-address-fetch r depth offset)
                        (setting-lookup r (vector-ref e 3)))))  ; e[3]: variable name
         (cond ((eq? value setting/missing) ; This can happen because interactive settings allow not-yet-defined variables.
                (signal k "Unbound variable" (vector-ref e 3)))
               ((eq? value uninitialized)
                (signal k "Uninitialized variable" (vector-ref e 3)))
               (else
                (answer k value))))))
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
           (answer k (object<- (script<- name #f clauses) ;TODO cache the script in the parsed 'make' exp. TODO miranda-trait instead of #f?
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
     (unpack p (depth offset name)
       (insist depth "Resolving an unbound variable?!" name)
       (cond ((setting-address-resolve! r depth offset name subject)
              => (lambda (plaint)
                   (signal k plaint name)))
             (else (answer k #t)))))
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

(define (run-script object script setting message k)
  (matching (script-clauses script) object script setting message k))

(define (matching clauses object script setting message k)
;  (dbg `(matching)) ; ,clauses))
  (mcase clauses
    (()
     (delegate (script-trait script) object message k))
    (((pattern pat-vars . body) . rest-clauses)
     (let ((pat-r (setting-extend-promises setting pat-vars)))
       (ev-pat message pattern pat-r
               (cont<- k-match-clause k pat-r body rest-clauses object script setting message))))))  ;XXX geeeez

(define (cont-match-clause matched? k0)
;     (dbg `(cont-match-clause))
  (unpack k0 (k pat-r body rest-clauses object script setting message)
               ;; TODO don't unpack it all till needed
  ;; body is now a list (body-vars body-exp)
    (if matched?
        (ev-exp (cadr body)
                (setting-extend-promises pat-r (car body))
                k)
        (matching rest-clauses object script setting message k))))

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


;; Install the primitives and the remaining primordia

(for-each (lambda (pair)
            (let ((key (car pair)) (value (cadr pair)))
              (setting-resolve! primordial-setting key value)))
          (append nonmeta-a-list
                  `(;; CPS primitives and other ties to interpreter internals.
                    ;; N.B. in primordia.scm there's a list you have to keep in sync.
                    (__raw-signal-handler-box ,raw-signal-handler-box)
                    (__evaluate ,evaluate-prim)
                    (error ,error-prim)
                    (with-ejector ,with-ejector-prim)
                    (__eject ,eject-prim)
                    (ejector-protect ,ejector-protect-prim)
                    (__reply ,reply-prim)
                    (__cps-primitive-name ,(lambda (x)
                                             (cps-script-name (object-script x))))
                    (extract-script ,extract-script)
                    (extract-datum ,extract-datum)
                    (__script-name ,script-name)
                    (__script-trait ,script-trait)
                    (__script-clauses ,script-clauses)
                    )))

(let ((elaborated-e (elaborate runtime primordial-setting)))
  (ev-exp elaborated-e primordial-setting halt-cont))

)
