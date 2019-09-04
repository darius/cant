#!chezscheme
(library (player primordia)
(export primordial-setting
        miranda-trait script/cps script/procedure script/ejector
        extract-script extract-datum
        runtime)
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
  (player nonmeta-primitives)
  )

(define trait-names '("miranda-trait"
                      "map-trait"
                      "list-trait"
                      "array-trait"))

(define type-names '(array
                     box
                     char
                     claim
                     cps
                     ejector
                     eof
                     link
                     map
                     nil
                     number
                     procedure
                     script
                     setting
                     sink
                     source
                     string
                     symbol
                     term
                     void))

(define (read-source parts)
  (let ((filename (string-append
                   (string-join "/" (list* "abcs" "00-primordia" parts))
                   ".cant")))
    (parse-exp `(do ,@(snarf filename cant-read)))))

(define code-a-list
  (map (lambda (name)
         (cons name (read-source (if (symbol? name)
                                     (list "types" (symbol->string name))
                                     (list name)))))
       (append trait-names type-names '("sugar"))))

(define runtime (read-source '("runtime")))

(define (ev-primordia e r)
  ((vector-ref methods/ev-primordia (pack-tag e))
   e r))

(define (ev-unsupported e r)
  (error 'ev-primordia "Transgressed beyond primordial Cant" e))

(define methods/ev-primordia
  (vector
   ev-unsupported                       ;e-constant
   ev-unsupported                       ;e-variable
   ev-unsupported                       ;e-term
   ev-unsupported                       ;e-list
   (lambda (e r)                        ;e-make
     (unpack e (name trait clauses)
       (if (eq? trait none-exp)
           (object<- (script<- name #f clauses)
                               r)
           (ev-unsupported e r))))
   (lambda (e r)                        ;e-do
     (unpack e (e1 e2)
       (ev-primordia e1 r)
       (ev-primordia e2 r)))
   (lambda (e r)                        ;e-let
     (unpack e (p e1)
       (let* ((value (ev-primordia e1 r))
              (matched? (ev-primordial-pat value p r)))
         (if matched?
             value
             (error 'ev-primordia "Match failure" value)))))
   ev-unsupported                       ;e-call
   ))

(define (ev-primordial-pat subject p r)
  ((vector-ref methods/ev-primordial-pat (pack-tag p))
   subject p r))

(define (ev-pat-unsupported subject e r)
  (error 'ev-primordial-pat "Transgressed beyond primordial Cant" e))

(define methods/ev-primordial-pat
  (vector
   ev-pat-unsupported                   ;p-constant
   (lambda (subject p r)                ;p-any
     #t)
   (lambda (subject p r)                ;p-variable
     (unpack p (name)
       (cond ((setting-resolve! r name subject)
              => (lambda (plaint)
                   (error 'ev-primordial-pat plaint name)))
             (else #t))))
   ev-pat-unsupported                   ;p-term
   ev-pat-unsupported                   ;p-list
   ev-pat-unsupported                   ;p-and
   ev-pat-unsupported                   ;p-view
   ))

(define primordial-setting
  (setting-ensure-bound
   (make-setting '())
   (append (map car nonmeta-a-list)
           '(__evaluate
             error
             with-ejector
             __eject
             ejector-protect
             __reply
             global-defined?
             __cps-primitive-name
             extract-script
             extract-datum
             __script-name
             __script-trait
             __script-clauses)
           (flatmap (lambda (pair)
                      (exp-vars-defined (cdr pair)))
                    code-a-list)
           (exp-vars-defined runtime))))

(define value-a-list
  (map (lambda (pair)
         (cons (car pair)
               (ev-primordia (elaborate (cdr pair) primordial-setting)
                             primordial-setting)))
       code-a-list))

(define (value-for name)
  (cdr (assoc name value-a-list)))

(define (script-for name)
  ;; TODO invoking the following script always goes through a failing
  ;; matching loop against the '() clauses before delegating to the
  ;; trait. Make it just delegate immediately.
  (script<- name (value-for name) '()))

(define miranda-trait    (value-for "miranda-trait"))

(define script/array     (script-for 'array))
(define script/box       (script-for 'box))
(define script/char      (script-for 'char))
(define script/claim     (script-for 'claim))
(define script/cps       (script-for 'cps))
(define script/ejector   (script-for 'ejector))
(define script/eof       (script-for 'eof))
(define script/link      (script-for 'link))
(define script/map       (script-for 'map))
(define script/nil       (script-for 'nil))
(define script/number    (script-for 'number))
(define script/procedure (script-for 'procedure))
(define script/script    (script-for 'script))
(define script/setting   (script-for 'setting))
(define script/sink      (script-for 'sink))
(define script/source    (script-for 'source))
(define script/string    (script-for 'string))
(define script/symbol    (script-for 'symbol))
(define script/term      (script-for 'term))
(define script/void      (script-for 'void))

(define (extract-script object)
  (cond
   ((number? object)      script/number)
   ((vector? object)      script/array)
   ((pair? object)        script/link)
   ((box? object)         script/box)
   ((string? object)      script/string)
   ((null? object)        script/nil)
   ((symbol? object)      script/symbol)
   ((output-port? object) script/sink)
   ((input-port? object)  script/source)
   ((char? object)        script/char)
   ((boolean? object)     script/claim)
   ((term? object)        script/term)
   ((mapi? object)        script/map)
   ((eq? object (void))   script/void)
   ((eof-object? object)  script/eof)
   ((script? object)      script/script)
   ((procedure? object)   script/procedure)
   ((setting? object)     script/setting)
   ((object? object)      (object-script object))
   (else (error 'call "Non-object" object))))

(define (extract-datum object)
  (cond
   ((object? object)      (object-datum object))
   ;; XXX: script too?
   (else                  object)))

)
