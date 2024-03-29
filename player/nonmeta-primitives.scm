;; Primitive definitions meant for primordial-setting.
;; The ones here are ordinary Scheme procedures or values; the
;; remaining ones added in primordia.scm/player.scm will have to do
;; with player internals.

(library (player nonmeta-primitives)
(export nonmeta-a-list)
(import (chezscheme)
  (player util)
  (player equality)
  (player source)
  (player read)
  (player parse)                        ;just for self-evaluating? ?
  (player setting)
  (player thing))

;; Compare primitives

(define (char-compare x y)
  (and (char? x) (char? y)      ;; XXX raise an error instead?
       (cond ((char<? x y) '<)
             ((char=? x y) '=)
             (else         '>))))

(define (number-compare x y)
  (and (number? x) (number? y)      ;; XXX raise an error instead?
       (cond ((< x y) '<)
             ((= x y) '=)
             (else    '>))))

(define (string-compare x y)
  (and (string? x) (string? y)      ;; XXX raise an error instead?
       (cond ((string<? x y) '<)
             ((string=? x y) '=)
             (else           '>))))

;; Misc primitives

(define (hashmap-place key keys none deleted)
  (let* ((m (vector-length keys))
         (mask (- m 1)))
    (let walking ((i0 (hash key))
                  (q 0)      ;iteration number for quadratic probing, d(q) = 0.5(q + q*q)
                  (slot #f)) ;if integer, then where to put the key if missing
      (let* ((i (logand mask i0))
             (k (vector-ref keys i)))
        (cond ((eq? k none)
               (term<- 'missing-at (or slot i)))
              ((cant=? k key)
               (term<- 'at i))
              ((= q m)
               (if slot
                   (term<- 'missing-at slot)
                   (error 'hashmap-place "Can't happen")))
              (else
               (walking (+ i (+ q 1))
                        (+ q 1)
                        (or slot (and (eq? k deleted) i)))))))))

(define (as-link x)
  (and (pair? x)
       (term<- 'link (car x) (cdr x))))

(define box<- box)
      
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

;; TODO reconcile with copy-range!
;; TODO range-check first
;; TODO no-op if in range and (dest,d) == (src,lo)
(define (vector-move! dest d source lo bound)
  (let ((diff (- d lo)))
    (if (<= d lo)
        (do ((i lo (+ i 1)))
            ((<= bound i))
          (vector-set! dest (+ i diff)
                       (vector-ref source i)))
        (do ((i (- bound 1) (- i 1)))
            ((< i lo))
          (vector-set! dest (+ i diff)
                       (vector-ref source i))))))

(define (maybe-macroexpand-expr e)
  (cond ((and (pair? e) (look-up-macro (car e)))
         => (lambda (expander)
              (term<- 'ok (expander e))))
        (else #f)))

(define (maybe-macroexpand-patt e)
  (cond ((and (pair? e) (look-up-pat-macro (car e)))
         => (lambda (expander)
              (term<- 'ok (expander e))))
        (else #f)))

(define (prim-halp-log start end result)
  (format #t "Halp ~w..~w: ~w\n" start end result) ;TODO actual format
  result)

(define (prim-nano-now)
  (let ((t (current-time)))
    (+ (* 1000000000 (time-second t))
       (time-nanosecond t))))

(define (prim-nanosleep nsec)
  (let* ((n (modulo nsec 1000000000))
         (nsec (- nsec n)))
    (sleep (make-time 'time-duration n (quotient nsec 1000000000)))))

(define (prim-*/mod n1 n2 d)
  (call-with-values (lambda () (div-and-mod (* n1 n2) d))
    (lambda (d m) (make-term '~ (list d m))))) ;TODO define tuple stuff in utils.scm

(define (prim-string-maps? me i)
  (and (integer? i)
       (< -1 i (string-length me))))

(define (prim-substring me lo bound)
  (if (< lo (string-length me))
      (substring me lo (min bound (string-length me)))
      ""))

(define (prim-vector-maps? me i)
  (and (integer? i)
       (< -1 i (vector-length me))))

(define (prim-display x sink)
  (cond ((or (char? x) (string? x) (symbol? x) (number? x))
         (display x sink)
         #t)
        (else #f)))

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


;; The defs for primordial-setting

(define (multiarg-cant=? . args)
  (or (null? args)
      (let ((arg1 (car args)))
;;        (all (lambda (arg) (cant=? arg1 arg)) (cdr args)))))
        (let loop ((rest (cdr args)))
          (or (null? rest)
              (and (cant=? arg1 (car rest))
                   (loop (cdr rest))))))))

(define nonmeta-a-list
  `((__as-link ,as-link)
    (= ,multiarg-cant=?)
    (in ,(make-source (current-input-port)))
    (out ,(current-output-port))
    (err ,(current-error-port))

    (link ,list*) ;;TODO insist that last argument = nil or pair? TODO is this n-arg form useful? 
    (link? ,pair?)
    (null? ,null?)
    (list? ,(lambda (x) (or (null? x) (pair? x))))
    (number? ,number?)
    (integer? ,integer?)
    (symbol? ,symbol?)
    (cue? ,cue?)
    (bool? ,boolean?)
    (rune? ,char?)
    (text? ,string?)
    (array? ,vector?)
    (box? ,box?)
    (term? ,term?)
    (source? ,source?) ;TODO these only know about primitive sources/sinks
    (sink? ,output-port?)
    (zilch? ,eof-object?)
    (box<- ,(case-lambda
              [() (box<- (void))]
              [(init) (box<- init)]))
    (symbol<- ,string->symbol)
    (term<- ,make-term)       ;TODO check that arguments arg is a list
    (rune<- ,integer->char)
    (__string<-list ,list->string)
    (array<-count ,make-vector)
    (inexact<-exact ,exact->inexact)  ;XXX rename or something
    (exact<-inexact ,inexact->exact)  ;XXX rename or something
    (floor ,floor)
    (ceiling ,ceiling)
    (round ,round)
    (not ,not)
    (assoc ,(lambda (x alist) (assp (lambda (key) (cant=? x key)) alist)))
    (sqrt ,sqrt)
    (exp ,exp)
    (log ,log)
    (panic ,prim-panic)
    (open-input-file ,(make-source-maker open-input-file)) ;XXX rename open-file-source
    (open-output-file ,open-output-file) ; open-file-sink
    ;; These binary file openers actually have more options in Chez
    ;; than just binary but let's just hack it in for now:
    (open-binary-input-file ,(make-source-maker open-file-input-port))
    (open-binary-output-file ,open-file-output-port)
    (__get-u8 ,source-get-u8)
    (__put-u8 ,put-u8)
;;    (__set-dbg! ,set-dbg!)
    (current-directory ,current-directory)
    (path-absolute? ,path-absolute?)
    (path-parent ,path-parent)
    (getenv ,getenv)

    ;; These will get high-level definitions later TODO
    (void ,(void))
    (/ ,/)
    (expt ,expt)
    (abs ,abs)
    (gcd ,gcd)
    (lcm ,lcm)
    (__array<-list ,list->vector)
    (read ,(lambda (source) (cant-read (source-port source))))
    (__parse-exp ,parse-exp)
    (__parse-pat ,parse-pat)
    (system ,system)
    ;; Should use string ports instead:
    (number<-text ,string->number)
    (text<-number ,number->string)
    (list<-text ,string->list)
    (text-source<- ,(make-source-maker open-input-string))
    (text-sink<- ,open-output-string)

    (__get-output-string ,get-output-string)
    (self-evaluating? ,self-evaluating?)
    (maybe-macroexpand-expr ,maybe-macroexpand-expr)
    (maybe-macroexpand-patt ,maybe-macroexpand-patt)
    (open-subprocess ,process/source)
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
    (__setting-inner-variables ,setting-inner-variables)
    (__setting-parent ,setting-parent)
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
    (__close-sink ,close-port)
    (__close-source ,close-source)
    (__read-char ,source-read-char)
    (__peek-char ,source-read-char)
    (__char-ready? ,source-char-ready?)
    (__read-all ,source-read-all)
    (__write-char ,write-char)
    (__display ,prim-display)
    (__depict ,depict)

    (__u+ ,(lambda (a b) (logand mask32 (+ a b)))) ;XXX revisit these definitions
    (__s+ ,(lambda (a b) (logand mask32 (+ a b)))) ;XXX I forget what distinction I meant to make
    (__u* ,(lambda (a b) ;XXX not really different from s* so far
             (unless (<= 0 a mask32) (error 'u* "Bad u32" a))
             (unless (<= 0 b mask32) (error 'u* "Bad u32" b))
             (logand mask32 (* a b))))
    (__s* ,(lambda (a b) (logand mask32 (* a b))))
    (__u- ,(lambda (a b) (logand mask32 (- a b))))
    (__u/ ,(lambda (a b)
             (unless (<= 0 a mask32) (error 'u/ "Bad u32" a))
             (unless (<= 0 b mask32) (error 'u/ "Bad u32" b))
             (logand mask32 (fx/ a b))))
    (__u<< ,(lambda (a b) (logand mask32 (ash a b))))
    (__u>> ,(lambda (a b) (logand mask32 (ash a (- b)))))

    (os-exit ,exit)
    ))

)
