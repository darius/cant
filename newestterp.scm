;; Interpreter

(include "gambit-macros.scm")

(define (run-load filename)
  (interpret `(do ,@(snarf filename squeam-read))))

(define (interpret e)
  (evaluate (parse-exp e) repl-env))

(define repl-env '())


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
        ((procedure? x) (receiver scheme-procedure-script x))
        (else (error "Non-object" x))))

(define boolean-script 'XXX)
(define number-script 'XXX)
(define symbol-script 'XXX)
(define nil-script 'XXX)
(define char-script 'XXX)
(define string-script 'XXX)
(define vector-script 'XXX)
(define procedure-script 'XXX)
(define pair-script 'XXX)
             
(define (answer k value)
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
                       (error "Not a script" script)))))))

(define (run-script object script datum message k)
  (let matching ((clauses (script-clauses script)))
    (mcase clauses
      (()
       (delegate (script-trait script) object message k))
      (((pattern body) . rest-clauses)
       (let ((pat-r (env-extend-promises datum (pat-vars-defined pattern))))
         (if (ev-pat message pattern pat-r k)
             (ev-exp body (env-extend-promises pat-r (exp-vars-defined body)) k)
             (matching rest-clauses)))))))

(define (delegate trait object message k)
  (cond ((object? trait)
         (call trait (list object message) k))
        ((not trait) ;XXX instead of #f use a special message-not-understood trait
         (signal k "Message not understood" object message))
        (else
         (error "Unknown script type" script))))

(define (signal k plaint . values)
  (error plaint values))                ;XXX


(define (as-cons x)
  (and (pair? x)
       (term<- 'cons (car x) (cdr x))))
      

;; Environments

(define the-global-env
  `((__as-cons ,as-cons)))

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
  equal?)

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

(define ev-trait-cont-script
  (make-cont-script
   (lambda (stamp-val k r trait clauses)
     (ev-exp trait r
             (cont<- ev-stamp-cont-script k stamp-val r clauses)))
   'XXX))

(define ev-stamp-cont-script
  (make-cont-script
   (lambda (trait-val k stamp-val r clauses)
     (answer k (object<- (script<- trait-val clauses) ;XXX use stamp-val
                         r)))
   'XXX))

(define ev-do-rest-cont
  (make-cont-script
   (lambda (_ k r e2)
     (ev-exp e2 r k))
   'XXX))

(define ev-let-match-cont
  (make-cont-script
   (lambda (val k r p)
     (ev-pat val p r
             (cont<- ev-let-check-cont k val)))
   'XXX))

(define ev-let-check-cont
  (make-cont-script
   (lambda (matched? k val)
     (if matched?
         (answer k val)
         (signal k "Match failure" val)))
   'XXX))

(define ev-arg-cont
  (make-cont-script
   (lambda (receiver k r e2)
     (ev-exp e2 r
             (cont<- ev-call-cont k receiver)))
   'XXX))

(define ev-call-cont
  (make-cont-script
   (lambda (message k receiver)
     (call receiver message k))
   'XXX))

(define ev-rest-args-cont
  (make-cont-script
   (lambda (val k es r vals)
     (ev-args es r (cons val vals) k))
   'XXX))

(define ev-tag-cont
  (make-cont-script
   (lambda (vals k tag)
     (answer k (make-term tag vals)))
   'XXX))

(define ev-and-pat-cont
  (make-cont-script
   (lambda (matched? k r subject p2)
     (if matched?
         (ev-pat subject p2 r k)
         (answer k #f)))
   'XXX))

(define ev-view-call-cont
  (make-cont-script
   (lambda (convert k r subject p)
     (call convert (list subject)
           (cont<- ev-view-match-cont k r p)))
   'XXX))

(define ev-view-match-cont
  (make-cont-script
   (lambda (new-subject k r p)
     (ev-pat new-subject p r k))
   'XXX))

(define ev-match-rest-cont
  (make-cont-script
   (lambda (matched? k r subjects ps)
     (if matched?
         (ev-match-all subjects ps r k)
         (answer k #f)))
   'XXX))
