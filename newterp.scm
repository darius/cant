;; Interpreter

;; Running a program

(define (boot)
  (run-load "newboot.scm"))

(define (run-load filename)
  (interpret `(do ,@(snarf filename squeam-read))))

(define (interpret e)
  (evaluate (parse-exp e) repl-env))

(define repl-env '())

(define (comparer<- < =)
  (lambda (a b)
    (cond ((< a b) '<)
          ((= a b) '=)
          ((< b a) '>))))

(define primitive-env
  `(
    (__nonempty-cont-parent ,car)

    (__let-cont-var ,cadr)              ;XXX or something

    (__claim-pick ,(lambda (claim if-yes if-no)
                     (if claim if-yes if-no)))

    (__int32+ ,+)
    (__int32- ,-)
    (__int32* ,*)
    (__int32-quotient ,quotient)
    (__int32-remainder ,remainder)
    (__int32-compare ,(comparer<- < =))
    (__int32<<   ,arithmetic-shift)
    (__int32-not ,bitwise-not)
    (__int32-and ,bitwise-and)
    (__int32-or  ,bitwise-ior)
    (__int32-xor ,bitwise-xor)

    (__symbol-name ,symbol->string)

    (__pair-first ,car)
    (__pair-rest  ,cdr)

    (__char-code        ,char->integer)
    (__char-letter?     ,char-alphabetic?)
    (__char-digit?      ,char-numeric?)
    (__char-whitespace? ,char-whitespace?)

    (__string-count ,string-length)
    (__string-ref   ,string-ref)
    (__string-maps? ,(lambda (me i) (and (integer? i)
                                         (< -1 i (string-length me)))))
    (__string-chain ,string-append)
    (__string-slice ,substring)

    (__vector-count ,vector-length)
    (__vector-ref   ,vector-ref)
    (__vector-maps? ,(lambda (me i) (and (integer? i)
                                         (< -1 i (vector-length me)))))
    (__vector-chain ,vector-append)
    (__vector-slice ,subvector)
    (__vector-set!  ,(lambda (me i val)
                       (vector-set! me i val)
                       #f))

    (__box-ref   ,(lambda (me) (vector-ref me 0)))
    (__box-set!  ,(lambda (me val)
                       (vector-set! me 0 val)
                       #f))
    ))


;; Objects, calling, and answering

(define-structure object script datum)
(define object<- make-object)

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
        ((object? x)    (receiver (object-script x) (object-datum x)))
        (else (error "Non-object" x))))
             
;; TODO: special-case this path through call for efficiency
(define (answer k value)
  (call k (list value) 'ignored))

(define (call object message k)
  (unwrap object
          (lambda (script datum)
            (cond
             ((procedure? script)
              (answer k (apply script object message)))
             ((pair? script)
              ;; An a-list of methods
              (cond ((assq cue script)
                     => (lambda (pair)
                          (apply (cadr pair) k datum arguments)))
                    (else
                     (message-not-found selector object k))))
             (else
              (error "Unknown script type" script))))))

(define (message-not-found cue object k)
  (signal k "No method found" cue object))

(define (signal k plaint . values)
  (let ((handler panic))  ;XXX (get-signal-handler)))
    ;; XXX install backstop handler or something, that'd act like:
    ;;     (apply error plaint values))
    (call handler (list k plaint values) halt-cont)))

(define (get-signal-handler)
  (call the-signal-handler-box '() halt-cont))

;; The default bare-bones signal handler.
(define (panic k plaint values)           ;XXX should be an object
  (apply error plaint values))


;; A small-step interpreter

(define (evaluate e r)
  (ev e r halt-cont))

(define (cont<- script . data)
  (object<- script data))

(define halt-cont
  (cont<- (lambda (value) value)))      ;XXX inadequate script, here and below

(define (ev e r k)
  (assert (term? e) "Unparsed expression" e)
  (let ((parts (term-parts e)))
    (case (term-tag e)
      ((variable)
       (env-lookup r (car parts) k))
      ((constant)
       (answer k (car parts)))
      ((make)
       (answer k (object<- (ev-make-script parts) r)))
      ((term)
       (let ((tag (car parts))
             (arg (cadr parts)))
         (ev arg r
             (cont<- term-cont-script k tag))))
      ((list)
       (ev-list (car parts) r k))
      ((let)
       (let ((p (car parts))
             (e1 (cadr parts)))
         (ev e1 r
             (cont<- let-cont-script k p r))))
      ((do)
       (let ((e1 (car parts))
             (e2 (cadr parts)))
         (ev e1 r
             (cont<- do-cont-script k e2))))
      ((call)
       (let ((addressee (car parts))
             (argument (cadr parts)))
         (ev addressee r
             (cont<- ev-arg-cont-script k argument))))
      (else
       (error "Unknown expression type" e)))))

(define let-cont-script
  (cont-script<-
   (lambda (value k p r)
     (ev-pat value p r 
       (cont<- let-value-cont-script k value)))))

(define let-value-cont-script
  (cont-script<-
   (lambda (match? k value)
     (if match?
         (answer k value)
         (signal k "Match failure" value)))))

(define (ev-pat value p r k)
  (assert (term? p) "Unparsed pattern" p)
  (let ((parts (term-parts p)))
    (case (term-tag p)
      ((any-pat)
       #yes)
      ((variable-pat)
       (let ((name (car parts)))
         ;(r .bind name subject)
         ;#yes)
         XXX))
      ((constant-pat)
       (let ((constant-value (car parts)))
         ;(= subject value))
         ;;XXX need to compare script and data
         (answer k (equal? value constant-value))))
      ((count-pat)
       (let ((n (car parts)))
         ;(and (list? subject) (= subject.count n)))
         XXX))
      ((prefix-pat)
       (let ((ps (car parts))
             (p-rest (cadr parts)))
         ;(and (list? subject) (match-prefix subject ps p-rest r)))
         XXX))
      ((term-pat)
       (let ((tag (car parts))
             (p-args (cadr parts)))
         ;(and (term? subject)
         ;     (eval-match subject.tag tag r)
         ;     (eval-match subject.arguments p-args r)))
         XXX))
      ((and-pat)
       (let ((p1 (car parts))
             (p2 (car parts)))
         ;(and (eval-match subject p1 r)
         ;     (eval-match subject p2 r)))
         XXX))
      ((view-pat)
       (let ((e (car parts))
             (p1 (car parts)))
         ;(eval-match (call (eval e (parent-only r))
         XXX))
      (else
       (error "Oops")))))
