(library (player nonmeta-primitives)
(export nonmeta-a-list)
(import (chezscheme)
  (player util)
  (player equality)
  (player read)
  (player parse)                        ;just for self-evaluating? ?
  (player setting)
  (player thing)
  (player primitives))

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

(define mask32 (- (expt 2 32) 1))

(define (prim-panic . arguments)
  (let ((message-for-chez ;Chez Scheme is picky about arguments to (error).
         (if (and (pair? arguments) (string? (car arguments)))
             arguments
             (cons "Error" arguments))))
    (apply error 'panic message-for-chez)))

;; TODO moveme
(define (prim-setting-resolve! setting variable value)
  (cond ((setting-resolve! setting variable value)
         => (lambda (plaint)
              (error '__setting-resolve! plaint variable)))
        (else #t)))

(define (prim-setting-lookup setting variable)
  (let ((value (setting-lookup setting variable)))
    (if (eq? value setting/missing)
        (error 'setting-lookup "Unbound variable" variable)
        value)))

(define nonmeta-a-list
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
    (panic ,prim-panic)
    (open-input-file ,open-input-file)  ;XXX rename open-file-source
    (open-output-file ,open-output-file) ; open-file-sink
    (open-binary-file-source ,open-file-input-port) ; This actually has more options in Chez than just binary
    (open-binary-file-sink ,open-file-output-port)  ; but let's just hack it in for now. 
    (__get-u8 ,get-u8)
    (__put-u8 ,put-u8)
;;    (__set-dbg! ,set-dbg!)
    (current-directory ,current-directory)
    (path-absolute? ,path-absolute?)
    (path-parent ,path-parent)

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
    (__halp-log ,prim-halp-log)
    (nano-now ,prim-nano-now)
    (nanosleep ,prim-nanosleep)
    (setting? ,setting?)
    (immutable-map? ,mapi?)             ;TODO sheesh the name
    (map<-items ,prim-mapi<-items)

    ;; Primitives only -- TODO seclude in their own env:
    (empty-setting ,empty-setting)
    (__setting<- ,make-setting)
    (__setting-extend-mutable ,setting-extend-mutable)
    (__setting-lookup ,prim-setting-lookup)
    (__setting-extend-promises ,setting-extend-promises)
    (__setting-resolve! ,prim-setting-resolve!)
    (__setting-binds? ,setting-binds?)
    (__setting-extend ,setting-extend)
    (__setting-variables ,setting-variables)
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

    (os-exit ,exit)
    ))

)
