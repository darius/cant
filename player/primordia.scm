#!chezscheme

;; A special interpreter just for bootstrapping the abcs/00-primordia/
;; code, since player.scm relies on that code being already installed.
;; (We used to use player.scm for bootstrapping too, but that sometimes
;; made debugging very painful when the bootstrap failed. Chez Scheme 
;; really doesn't like that kind of circularity, which is fair enough.)

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
  (player source)
  (player ast)
  (player parse)
  (player setting)
  (player thing)
  (player elaborate)
  (player nonmeta-primitives)
  )

(define trait-names '("miranda-trait"
                      "array-trait"
                      "list-trait"
                      "map-trait"
                      "sink-trait"))

(define type-names '(array
                     box
                     bool
                     cps
                     ejector
                     link
                     map
                     nil
                     number
                     procedure
                     rune
                     script
                     setting
                     sink
                     source
                     symbol
                     term
                     text
                     void
                     zilch))

(define base-path (getenv "CANT_DIR"))       ;TODO duplicated in abcs.scm

(define (read-source parts)
  (let ((filename (string-append
                   (string-join "/" (list* base-path "abcs" "00-primordia" parts))
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
     (unpack p (_depth _offset name)
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
   (setting-extend-mutable #f)
   (append (map car nonmeta-a-list)
           '(__raw-signal-handler-box
             __evaluate
             oops
             with-ejector
             __eject
             ejector-protect
             __reply
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
(define script/bool      (script-for 'bool))
(define script/cps       (script-for 'cps))
(define script/ejector   (script-for 'ejector))
(define script/link      (script-for 'link))
(define script/map       (script-for 'map))
(define script/nil       (script-for 'nil))
(define script/number    (script-for 'number))
(define script/procedure (script-for 'procedure))
(define script/rune      (script-for 'rune))
(define script/script    (script-for 'script))
(define script/setting   (script-for 'setting))
(define script/sink      (script-for 'sink))
(define script/source    (script-for 'source))
(define script/symbol    (script-for 'symbol))
(define script/term      (script-for 'term))
(define script/text      (script-for 'text))
(define script/void      (script-for 'void))
(define script/zilch     (script-for 'zilch))

(define (extract-script object)
  (cond
   ((number? object)      script/number)
   ((vector? object)      script/array)
   ((pair? object)        script/link)
   ((box? object)         script/box)
   ((string? object)      script/text)
   ((null? object)        script/nil)
   ((symbol? object)      script/symbol)
   ((output-port? object) script/sink)
   ((source? object)      script/source)
   ((char? object)        script/rune)
   ((boolean? object)     script/bool)
   ((term? object)        script/term)
   ((mapi? object)        script/map)
   ((eq? object (void))   script/void)
   ((eof-object? object)  script/zilch)
   ((script? object)      script/script)
   ((procedure? object)   script/procedure)
   ((setting? object)     script/setting)
   ((object? object)
    ;; Being tricky here. There are two users of extract-script.
    ;; One is player.scm's call procedure, and it always handles the 
    ;; (object? object) case itself, in its own way.
    ;; The other user is Cant's debugger. We don't want to give it a 
    ;; cps-script Scheme object, because that's not a Cant object.
    ;; Here we give it script/cps, instead.
    ;; TODO be not-tricky
    (let ((script (object-script object)))
      (if (cps-script? script)
          script/cps
          script)))
   (else (error 'extract-script "Non-object" object))))

;; The only user of extract-datum is Cant's debugger. We special-case
;; cps-primitives as above for extract-script.
(define (extract-datum object)
  (cond
   ((object? object)
    (if (cps-script? (object-script object))
        ;; Since there's nothing useful in a cps-primitive's datum:
        ;;   (TODO maybe that's where we should stash the name?)
        (cps-script-name (object-script object))  
        (object-datum object)))
   (else object)))


)
