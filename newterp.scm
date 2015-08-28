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

(define halt-cont
  'XXX)
