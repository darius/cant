;; Interpreter

(include "gambit-macros.scm")

(define (run-load filename)
  (interpret `(do ,@(snarf filename squeam-read))))

(define (interpret e)
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

(define (hash x)
  (cond ((term? x) (hash-term x))
        ((pair? x) (hash-pair x))
        ((string? x) (hash-string x))
        (else (eqv?-hash x))))

(define (hash-term x)
  (hash-em 1 (cons (term-tag x) (term-parts x))))

(define (hash-pair x)
  (hash-em 2 (list (car x) (cdr x))))

(define (hash-string x)
  (hash-em 3 (string->list x)))         ;TODO find a built-in string hash fn?

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

(define-structure object script datum)   ; Nonprimitive objects, that is.
(define object<- make-object)

(define-structure script name trait clauses)
(define script<- make-script)

(define (answer k value)
;  (dbg `(answer ,value ,k))
;  (dbg `(answer))
  ;; This could be just (call k (term<- '.answer value) 'ignored))
  ;; but it's specialized here to recover a little lost speed.
  (if (object? k)
      (let ((script (object-script k)))
        (if (cont-script? script)
            (apply (cont-script-answerer script)
                   value
                   (object-datum k))
            (error "Answering to a non-cont" k)))
      (error "Answering to a non-cont" k)))

(define (call object message k)
;  (dbg `(call))
  (cond ((procedure? object)
         (if (or (pair? message) (null? message))
             (cond ((eq? object error-prim) (error-prim (cons k message)))
                   ((eq? object evaluate-prim) (evaluate-prim message k))
                   (else (answer k (apply object message))))
             (run-script object procedure-script object message k)))
        ((object? object)
         (let ((script (object-script object))
               (datum (object-datum object)))
           (cond ((script? script)
                  (run-script object script datum message k))
                 ((cont-script? script)
                  (if (and (term? message)
                           (eq? '.answer (term-tag message))
                           (= 1 (length (term-parts message))))
                      (apply (cont-script-answerer script)
                             (cons (car (term-parts message)) datum))
                      (call-cont-standin script datum message k)))
                 (else
                  (error "Not a script" script datum)))))
        (else
         (let ((script
                (cond
                 ((number? object)      number-script)
                 ((string? object)      string-script)
                 ((pair? object)        pair-script)
                 ((vector? object)      vector-script)
                 ((box? object)         box-script)
                 ((null? object)        nil-script)
                 ((input-port? object)  source-script)
                 ((output-port? object) sink-script)
                 ((symbol? object)      symbol-script)
                 ((boolean? object)     boolean-script)
                 ((char? object)        char-script)
                 ((term? object)        term-script)
                 ((eq? object (void))   void-script)
                 (else (error "Non-object" object)))))
           (run-script object script object message k)))))

;; XXX This is a hack.
(define (call-cont-standin script datum message k)
;  (pp `(making standin ,(cont-script-name script)))
  (let ((make-standin (get-prim (cont-script-name script))))
    (call make-standin datum
          (cont<- call-cont-standin-cont k message))))

(define (error-prim message)
  (let* ((the-box (get-prim 'the-signal-handler-box))
         (handler (call the-box (term<- '.^) halt-cont)))
    ;; Panic by default if another error occurs during error handling.
    (call the-box (term<- '.^= panic) halt-cont)
    ;; OK, up to the handler now.
    (call handler message halt-cont)))

(define (panic k . message)
  (apply error message))

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
                       (else (error "Unknown script type" script)))))
    (call handler (list object message) k)))

(define (signal k plaint . values)
  (error-prim `(,k ,plaint ,@values)))


(define (as-cons x)
  (and (pair? x)
       (term<- 'cons (car x) (cdr x))))

(define-structure box value)
(define box<- make-box)
      
(define (evaluate-prim message k)
  (apply ev-exp `(,@message ,k)))


;; Environments

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
    (symbol? ,symbol?)
    (char? ,char?)
    (string? ,string?)
    (vector? ,vector?)
    (box? ,box?)
    (source? ,input-port?)
    (sink? ,output-port?)
    (eof-object? ,eof-object?)
    (box<- ,box<-)
    (symbol<- ,string->symbol)
    (term<- ,make-term)       ;TODO check that arguments arg is a list
    (char<- ,integer->char)
    (string<-list ,list->string)
    (vector<-count ,make-vector)
    (not ,not)
    (assq ,assq)  ;; TODO replace with 'real' hashmaps
    (assoc ,assoc)  ;; TODO replace with 'real' hashmaps
    (display ,display)           ;XXX temp
    (newline ,newline)           ;XXX temp
    (pp ,pp)                     ;XXX obviously shouldn't be primitive
    (panic ,panic)
    (error ,error-prim)
    (evaluate ,evaluate-prim)
    (open-input-file ,open-input-file)
    (__set-dbg! ,set-dbg!)

    ;; These will get high-level definitions later TODO
    (/ ,/)
    (vector<-list ,list->vector)
    (read ,squeam-read)
    (parse-exp ,parse-exp)
    (parse-pat ,parse-pat)
    (random-integer ,random-integer)
    ;; Should use string ports instead:
    (number<-string ,string->number)
    (string<-number ,number->string)
    (read ,squeam-read)

    ;; Primitives only -- TODO seclude in their own env:
    (__hash ,hash)
    (__char-compare ,char-compare)
    (__number-compare ,number-compare)
    (__+ ,+)
    (__- ,-)
    (__* ,*)
    (__quotient ,quotient)
    (__remainder ,remainder)
;    (__number-compare
    (__bit-<< ,arithmetic-shift)
    (__bit->> ,(lambda (x y) (arithmetic-shift x (- y))))
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
    (__string-compare ,string-compare)
    (__string-length ,string-length)
    (__string-maps? ,(lambda (me i)
                       (and (integer? i)
                            (< -1 i (string-length me)))))
    (__string-ref ,string-ref)
    (__substring ,(lambda (me lo bound)
                    (substring me lo (min bound (string-length me)))))
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
    (__box-value ,box-value)
    (__box-value-set! ,box-value-set!)
    (__term-tag ,term-tag)
    (__term-arguments ,term-parts)
    (__close-port ,close-port)
    (__read-char ,read-char)
    (__read-all ,(lambda (port)
                   (let reading ((cs '()))
                     (let ((c (read-char port)))
                       (if (eof-object? c)
                           (if (null? cs)
                               c
                               (list->string (reverse cs)))
                           (reading (cons c cs)))))))
    (__write-char ,write-char)
    (__display ,(lambda (sink thing)
                  (display thing sink)))              ;XXX handle non-string/char properly
    (__write ,(lambda (sink thing)
                (cond ((object? thing)
                       (display "#<" sink)
                       (let ((script (object-script thing)))
                         (cond ((script? script) (write (script-name script)))
                               ((cont-script? script) (write (cont-script-name script)))
                               (else (display "WTF"))))
                       (display ">" sink))
                      (else (write thing sink))))) ;XXX other types specially?

    (__u+ ,(lambda (a b) (bitwise-and mask32 (+ a b)))) ;XXX revisit these definitions
    (__s+ ,(lambda (a b) (bitwise-and mask32 (+ a b)))) ;XXX I forget what distinction I meant to make
    (__s* ,(lambda (a b) (bitwise-and mask32 (* a b))))
    (__u- ,(lambda (a b) (bitwise-and mask32 (- a b))))
    (__u<< ,(lambda (a b) (bitwise-and mask32 (arithmetic-shift a b))))
    (__u>> ,(lambda (a b) (bitwise-and mask32 (arithmetic-shift a (- b)))))
    ))

(define mask32 (- 1 (expt 2 32)))

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
                         (assert (eq? (cadr pair) uninitialized)
                                 "Multiple definition" v)
                         (set-car! (cdr pair) value)))
        ((null? r)
         (assert (not (assq v the-global-env)) "Global redefinition" v)
         (set! the-global-env (cons (list v value) the-global-env)))
        (else (error "Can't happen" v))))

(define uninitialized (object<- (script<- '<uninitialized> #f '()) '*uninitialized*))


;; A small-step interpreter

(define (evaluate e r)
;  (report `(evaluate ,e))
  (ev-exp e r halt-cont))

(define (ev-exp e r k)
;  (dbg `(ev-exp)) ; ,e))
  (let ((parts (term-parts e)))
    (case (term-tag e)
      ((constant)
       (answer k (car parts)))
      ((variable)
       (env-lookup r (car parts) k))
      ((make)
       (let ((name (car parts))
             (stamp (cadr parts))
             (trait (caddr parts))
             (clauses (cadddr parts)))
         (if (and (eq? trait none-exp) ; Just fast-path tuning; this if is not logically necessary.
                  (eq? stamp none-exp))
             (answer k (object<- (script<- name #f clauses)
                                 r))
             (ev-exp stamp r
                     (cont<- ev-trait-cont-script k r name trait clauses)))))
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
;  (dbg `(match)) ; ,subject ,p))
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
(define-structure cont-script name answerer)

(define halt-cont
  (object<- (make-cont-script '__halt-cont (lambda (value) value))
            '()))

(set! the-global-env
      `((halt-cont ,halt-cont) ,@the-global-env)) ;XXX temp

(define call-cont-standin-cont          ;XXX still a hack
  (make-cont-script
   '__call-cont-standin-cont
   (lambda (standin k message)
;;     (pp `(call-cont-standin-cont ,message))
     (call standin message k))))

(define match-clause-cont
  (make-cont-script
   '__match-clause-cont
   (lambda (matched? k pat-r body rest-clauses object script datum message)
;     (dbg `(match-clause-cont))
     ;; body is now a list (body-vars body-exp)
     (if matched?
         (ev-exp (cadr body) (env-extend-promises pat-r (car body)) k)
         (matching rest-clauses object script datum message k)))))

(define ev-trait-cont-script
  (make-cont-script
   '__ev-trait-cont
   (lambda (stamp-val k r name trait clauses)
;     (dbg `(ev-trait-cont))
     (ev-exp trait r
             (cont<- ev-make-cont-script k name stamp-val r clauses)))))

(define ev-make-cont-script
  (make-cont-script
   '__ev-make-cont
   (lambda (trait-val k name stamp-val r clauses)
;     (dbg `(ev-make-cont))
     (answer k (object<- (script<- name trait-val clauses) ;XXX use stamp-val
                         r)))))

(define ev-do-rest-cont
  (make-cont-script
   '__ev-do-rest-cont
   (lambda (_ k r e2)
;     (dbg `(ev-do-rest-cont))
     (ev-exp e2 r k))))

(define ev-let-match-cont
  (make-cont-script
   '__ev-let-match-cont
   (lambda (val k r p)
;     (dbg `(ev-let-match-cont))
     (ev-pat val p r
             (cont<- ev-let-check-cont k val)))))

(define ev-let-check-cont
  (make-cont-script
   '__ev-let-check-cont
   (lambda (matched? k val)
;     (dbg `(ev-let-check-cont))
     (if matched?
         (answer k val)
         (signal k "Match failure" val)))))

(define ev-arg-cont
  (make-cont-script
   '__ev-arg-cont
   (lambda (receiver k r e2)
;     (dbg `(ev-arg-cont))
     (ev-exp e2 r
             (cont<- ev-call-cont k receiver)))))

(define ev-call-cont
  (make-cont-script
   '__ev-call-cont
   (lambda (message k receiver)
;     (dbg `(ev-call-cont ,receiver ,message))
     (call receiver message k))))

(define ev-rest-args-cont
  (make-cont-script
   '__ev-rest-args-cont
   (lambda (val k es r vals)
;     (dbg `(ev-rest-args-cont))
     (ev-args es r (cons val vals) k))))

(define ev-tag-cont
  (make-cont-script
   '__ev-tag-cont
   (lambda (vals k tag)
;     (dbg `(ev-tag-cont))
     (answer k (make-term tag vals)))))

(define ev-and-pat-cont
  (make-cont-script
   '__ev-and-pat-cont
   (lambda (matched? k r subject p2)
;     (dbg `(ev-and-pat-cont))
     (if matched?
         (ev-pat subject p2 r k)
         (answer k #f)))))

(define ev-view-call-cont
  (make-cont-script
   '__ev-view-call-cont
   (lambda (convert k r subject p)
;     (dbg `(ev-view-call-cont))
     (call convert (list subject)
           (cont<- ev-view-match-cont k r p)))))

(define ev-view-match-cont
  (make-cont-script
   '__ev-view-match-cont
   (lambda (new-subject k r p)
;     (dbg `(ev-view-match-cont))
     (ev-pat new-subject p r k))))

(define ev-match-rest-cont
  (make-cont-script
   '__ev-match-rest-cont
   (lambda (matched? k r subjects ps)
;     (dbg `(ev-match-rest-cont))
     (if matched?
         (ev-match-all subjects ps r k)
         (answer k #f)))))


;; Primitive types

(define primitive-env '())

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
(define vector-script (get-script 'vector-primitive))
(define box-script    (get-script 'box-primitive))
(define source-script (get-script 'source-primitive))
(define sink-script   (get-script 'sink-primitive))
(define term-script   (get-script 'term-primitive))
(define procedure-script (get-script 'procedure-primitive))
(define void-script   (get-script 'void-primitive))


;; For tuning later.

(define (report-stats)
  'ok)
