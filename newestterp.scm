;; Interpreter

(include "gambit-macros.scm")

(define (run-load filename)
  (interpret `(do ,@(snarf filename squeam-read))))

(define (interpret e)
  (evaluate (parse-exp e) repl-env))

(define repl-env '())

(define (dbg x)
  #f)


(define squeam=?
  ;; For now, I'm gonna assume Squeam-defined objects are equal iff
  ;; eq?. This means you can't reconstitute an object from its script
  ;; and datum, which would be a reasonable implementation-level
  ;; operation for which squeam=? would check if script and datum
  ;; are eq? -- but then we'd have to walk over lists and terms if
  ;; they might contain these objects that are squeam=? but not eq?.
  ;; N.B. this depends on equal? working with define-structure, for
  ;; equality over terms.
  ;; XXX Actually that makes it too willing to call objects equal --
  ;; it'll also look inside a Squeam-defined object since it's a
  ;; structure. Let's just live with that for now in this prototype.
  ;; TODO actually define-type (but not define-structure) apparently
  ;; has an 'opaque' option to override that -- come back and see.
  equal?)


;; Objects, calling, and answering

(define-structure object script datum)   ; Nonprimitive objects, that is.
(define object<- make-object)

(define-structure script trait clauses)
(define script<- make-script)

(define (unwrap x receiver)
  (cond ((object? x)    (receiver (object-script x) (object-datum x)))
        ((boolean? x)   (receiver boolean-script x))
        ((number? x)    (receiver number-script x))
        ((symbol? x)    (receiver symbol-script x))
        ((null? x)      (receiver nil-script x))
        ((pair? x)      (receiver pair-script x))
        ((char? x)      (receiver char-script x))
        ((string? x)    (receiver string-script x))
        ((vector? x)    (receiver vector-script x))
        ((box? x)       (receiver box-script x))
        ((output-port? x) (receiver sink-script x))
        ((procedure? x) (receiver scheme-procedure-script x))
        (else (error "Non-object" x))))

(define (answer k value)
  (dbg `(answer ,value ,k))
  (call k (list value) 'ignored))

(define (call object message k)
  (if (and (procedure? object)
           (list? message))
      (answer k (apply object message))
      (unwrap object
              (lambda (script datum)
                (cond ((script? script)
                       (run-script object script datum message k))
                      ((cont-script? script)
                       (if (and (pair? message) (null? (cdr message)))
                           (apply (cont-script-answerer script)
                                  (car message)
                                  datum)
                           (run-script object (cont-script-script script)
                                       datum message k)))
                      (else
                       (error "Not a script" script datum)))))))

(define (run-script object script datum message k)
  (matching (script-clauses script) object script datum message k))

(define (matching clauses object script datum message k)
  (dbg `(matching ,clauses))
  (mcase clauses
    (()
     (delegate (script-trait script) object message k))
    (((pattern body) . rest-clauses)
     (let ((pat-r (env-extend-promises datum (pat-vars-defined pattern))))
       (ev-pat message pattern pat-r
               (cont<- match-clause-cont k pat-r body rest-clauses object script datum message))))))  ;XXX geeeez

(define (delegate trait object message k)
  (dbg `(delegate))
  (let ((handler (cond ((object? trait) trait)
                       ((not trait) miranda-trait)
                       (else (error "Unknown script type" script)))))
    (call handler (list object message) k)))

(define (signal k plaint . values)
  (error plaint values))                ;XXX


(define (as-cons x)
  (and (pair? x)
       (term<- 'cons (car x) (cdr x))))

(define-structure box value)
(define box<- make-box)
      

;; Environments

(define the-global-env
  `((__as-cons ,as-cons)
    (= ,squeam=?)
    (out ,(current-output-port))

    (cons ,cons)
    (null? ,null?)
    (list? ,(lambda (x) (or (null? x) (pair? x))))
    (number? ,number?)
    (symbol? ,symbol?)
    (char? ,char?)
    (string? ,string?)
    (vector? ,vector?)
    (box? ,box?)
    (sink? ,output-port?)
    (box<- ,box<-)
    (symbol<- ,string->symbol)
    (term<- ,make-term)
    (string<-list ,list->string)
    (< ,<)  ;; XXX use 'compare method instead
    (<= ,<=)
    (vector<-count ,make-vector)
    (not ,not)
    (assq ,assq)  ;; TODO replace with 'real' hashmaps
    (display ,display)
    (write ,write)                      ;XXXtemporary
    (newline ,newline)
    (pp ,pp)                     ;XXX obviously shouldn't be primitive

    ;; These will get high-level definitions later TODO
    (error ,error)
    (+ ,+)
    (- ,-)
    (* ,*)
    (/ ,/)
    (max ,max)
    (number<-string ,string->number)
    (vector<-list ,list->vector)

    ;; Primitives only -- TODO seclude in their own env:
    (__+ ,+)
    (__- ,-)
    (__* ,*)
    (__quotient ,quotient)
    (__remainder ,remainder)
;    (__number-compare
    (__bit-<< ,arithmetic-shift)
    (__bit-not ,bitwise-not)
    (__bit-and ,bitwise-and)
    (__bit-or  ,bitwise-ior)
    (__bit-xor ,bitwise-xor)
    (__car ,car)
    (__cdr ,cdr)
    (__append ,append)
    (__length ,length)
    (__list-ref ,list-ref)
    (__append ,append)
    (__symbol->string ,symbol->string)
    (__string-append ,string-append)
    (__string-length ,string-length)
    (__string-maps? ,(lambda (me i)
                       (and (integer? i)
                            (< -1 i (string-length me)))))
    (__string-ref ,string-ref)
    (__substring ,substring)
    (__vector-append ,vector-append)
    (__vector-length ,vector-length)
    (__vector-maps? ,(lambda (me i)
                       (and (integer? i)
                            (< -1 i (vector-length me)))))
    (__vector-ref ,vector-ref)
    (__vector-set! ,vector-set!)
    (__vector->list ,vector->list)
    (__subvector ,subvector)
    (__char->integer ,char->integer)
    (__char-digit? ,char-numeric?)
    (__char-letter? ,char-alphabetic?)
    (__char-whitespace? ,char-whitespace?)
    (__box-value ,box-value)
    (__box-value-set! ,box-value-set!)
    (__display ,(lambda (sink thing)
                  (display thing sink)))              ;XXX handle non-string/char properly
    (__write ,(lambda (sink thing)
                  (write thing sink)))              ;XXX handle non-primitive properly?
    ))

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

(define uninitialized (object<- (script<- #f '()) '*uninitialized*))


;; Variables defined

(define (pat-vars-defined p)
  (let ((parts (term-parts p)))
    (case (term-tag p)
      ((any-pat constant-pat)
       '())
      ((variable-pat)
       parts)
      ((term-pat)
       (let ((p-args (cadr parts)))
         (flatmap pat-vars-defined p-args)))
      ((and-pat)
       (let ((p1 (car parts)) (p2 (cadr parts)))
         (append (pat-vars-defined p1)
                 (pat-vars-defined p2))))
      ((view-pat)
       (let ((e (car parts)) (p (cadr parts)))
         (append (exp-vars-defined e)
                 (pat-vars-defined p))))
      (else
       (error "Bad pattern type" p)))))

(define (exp-vars-defined e)
  (let ((parts (term-parts e)))
    (case (term-tag e)
      ((constant variable make)
       '())
      ((call do)
       (let ((e1 (car parts)) (e2 (cadr parts)))
         (append (exp-vars-defined e1)
                 (exp-vars-defined e2))))
      ((let)
       (let ((p (car parts)) (e (cadr parts)))
         (append (pat-vars-defined p)
                 (exp-vars-defined e))))
      ((term)
       (let ((es (cadr parts)))
         (flatmap exp-vars-defined es)))
      ((list)
       (let ((es (car parts)))
         (flatmap exp-vars-defined es)))
      (else
       (error "Bad expression type" e)))))


;; A small-step interpreter

(define (evaluate e r)
;  (print `(evaluate ,e))
  (ev-exp e r halt-cont))

(define (ev-exp e r k)
  (dbg `(ev ,e))
  (let ((parts (term-parts e)))
    (case (term-tag e)
      ((constant)
       (answer k (car parts)))
      ((variable)
       (env-lookup r (car parts) k))
      ((make)
       (let ((stamp (car parts))
             (trait (cadr parts))
             (clauses (caddr parts)))
         (ev-exp stamp r
                 (cont<- ev-trait-cont-script k r trait clauses))))
      ((do)
       (let ((e1 (car parts)) (e2 (cadr parts)))
         (ev-exp e1 r (cont<- ev-do-rest-cont k r e2))))
      ((let)
       (let ((p (car parts)) (e1 (cadr parts)))
         (ev-exp e1 r (cont<- ev-let-match-cont k r p))))
      ((call)
       (let ((e1 (car parts)) (e2 (cadr parts)))
         (ev-exp e1 r (cont<- ev-arg-cont k r e2))))
      ((term)
       (let ((tag (car parts)) (es (cadr parts)))
         (ev-args es r '()
                  (cont<- ev-tag-cont k tag))))
      ((list)
       (let ((es (car parts)))
         (ev-args es r '() k)))
      (else
       (error "Bad exp type" e)))))

(define (ev-args es r vals k)
  (if (null? es)
      (answer k (reverse vals))
      (ev-exp (car es) r
              (cont<- ev-rest-args-cont k (cdr es) r vals))))

(define (ev-pat subject p r k)
  (dbg `(match ,subject ,p))
  (let ((parts (term-parts p)))
    (case (term-tag p)
      ((constant-pat)
       (let ((value (car parts)))
         (answer k (squeam=? subject value))))
      ((any-pat)
       (answer k #t))
      ((variable-pat)
       (let ((name (car parts)))
         (env-resolve! r name subject)  ;XXX properly signal any error
         (answer k #t)))
      ((term-pat)
       (let ((tag (car parts)) (p-args (cadr parts)))
         (if (not (and (term? subject)
                       (squeam=? (term-tag subject) tag)))
             (answer k #f)
             (ev-match-all (term-parts subject) p-args r k))))
      ((and-pat)
       (let ((p1 (car parts)) (p2 (cadr parts)))
         (ev-pat subject p1 r
                 (cont<- ev-and-pat-cont k r subject p2))))
      ((view-pat)
       (let ((e (car parts)) (p (cadr parts)))
         (ev-exp e r
                 (cont<- ev-view-call-cont k r subject p))))
      (else
       (error "Bad pattern type" p)))))

(define (ev-match-all subjects ps r k)
  (cond ((null? ps)
         (answer k (null? subjects)))
        ((null? subjects)
         (answer k #f))
        (else
         (ev-pat (car subjects) (car ps) r
                 (cont<- ev-match-rest-cont k r (cdr subjects) (cdr ps))))))

(define (cont<- cont-script k . values)
  (object<- cont-script (cons k values)))

;; Continuation scripts are special mainly for efficiency at answering
;; to continuations. It also saves us from having to name and bind,
;; for each continuation type, a separate primitive procedure to do
;; the actual answering -- and those'd have to be a special kind of
;; primitive that doesn't need a continuation, too.
(define-structure cont-script answerer script)

(define halt-cont
  (cont<- (make-cont-script (lambda (value _) value) 'XXX)
          #f))

(define match-clause-cont
  (make-cont-script
   (lambda (matched? k pat-r body rest-clauses object script datum message)
     (dbg `(match-clause-cont))
     (if matched?
         (ev-exp body (env-extend-promises pat-r (exp-vars-defined body)) k)
         (matching rest-clauses object script datum message k)))
   'XXX))

(define ev-trait-cont-script
  (make-cont-script
   (lambda (stamp-val k r trait clauses)
     (dbg `(ev-trait-cont))
     (ev-exp trait r
             (cont<- ev-make-cont-script k stamp-val r clauses)))
   'XXX))

(define ev-make-cont-script
  (make-cont-script
   (lambda (trait-val k stamp-val r clauses)
     (dbg `(ev-make-cont))
     (answer k (object<- (script<- trait-val clauses) ;XXX use stamp-val
                         r)))
   'XXX))

(define ev-do-rest-cont
  (make-cont-script
   (lambda (_ k r e2)
     (dbg `(ev-do-rest-cont))
     (ev-exp e2 r k))
   'XXX))

(define ev-let-match-cont
  (make-cont-script
   (lambda (val k r p)
     (dbg `(ev-let-match-cont))
     (ev-pat val p r
             (cont<- ev-let-check-cont k val)))
   'XXX))

(define ev-let-check-cont
  (make-cont-script
   (lambda (matched? k val)
     (dbg `(ev-let-check-cont))
     (if matched?
         (answer k val)
         (signal k "Match failure" val)))
   'XXX))

(define ev-arg-cont
  (make-cont-script
   (lambda (receiver k r e2)
     (dbg `(ev-arg-cont))
     (ev-exp e2 r
             (cont<- ev-call-cont k receiver)))
   'XXX))

(define ev-call-cont
  (make-cont-script
   (lambda (message k receiver)
     (dbg `(ev-call-cont ,receiver ,message))
     (call receiver message k))
   'XXX))

(define ev-rest-args-cont
  (make-cont-script
   (lambda (val k es r vals)
     (dbg `(ev-rest-args-cont))
     (ev-args es r (cons val vals) k))
   'XXX))

(define ev-tag-cont
  (make-cont-script
   (lambda (vals k tag)
     (dbg `(ev-tag-cont))
     (answer k (make-term tag vals)))
   'XXX))

(define ev-and-pat-cont
  (make-cont-script
   (lambda (matched? k r subject p2)
     (dbg `(ev-and-pat-cont))
     (if matched?
         (ev-pat subject p2 r k)
         (answer k #f)))
   'XXX))

(define ev-view-call-cont
  (make-cont-script
   (lambda (convert k r subject p)
     (dbg `(ev-view-call-cont))
     (call convert (list subject)
           (cont<- ev-view-match-cont k r p)))
   'XXX))

(define ev-view-match-cont
  (make-cont-script
   (lambda (new-subject k r p)
     (dbg `(ev-view-match-cont))
     (ev-pat new-subject p r k))
   'XXX))

(define ev-match-rest-cont
  (make-cont-script
   (lambda (matched? k r subjects ps)
     (dbg `(ev-match-rest-cont))
     (if matched?
         (ev-match-all subjects ps r k)
         (answer k #f)))
   'XXX))


;; Primitive types

(define primitive-env '())

(define (get-script name)
  (script<- (get-prim name) primitive-env))

(define (get-prim name)
  (env-lookup primitive-env name halt-cont))

(run-load "runtime.scm")

(define miranda-trait (get-prim 'miranda-trait))

(define boolean-script (get-script 'claim-primitive))

(define number-script (get-script 'number-primitive))

(define nil-script    (get-script 'list-primitive))
(define pair-script   (get-script 'list-primitive))

(define symbol-script (get-script 'symbol-primitive))
(define char-script   (get-script 'char-primitive))
(define string-script (get-script 'string-primitive))
(define vector-script (get-script 'vector-primitive))
(define box-script    (get-script 'box-primitive))
(define sink-script   (get-script 'sink-primitive))
(define procedure-script (get-script 'procedure-primitive))
