;; Top level

(let the-signal-handler (box<- panic))
(let the-last-error (box<- #no))

(to (push-signal-handler handler)
  (let parent-handler the-signal-handler.^)
  (to (handler/wrapped @evil)
    ;; TODO might be cleaner with unwind-protect actions spelled out inline
    (unwind-protect
     (to (handler-thunk)
       (the-signal-handler .^= parent-handler)
       (handler @evil))
     (to (unwind-thunk)
       (the-signal-handler .^= handler/wrapped))))
  (the-signal-handler .^= handler/wrapped))

(to (with-signal-handler handler thunk)
  (let parent-handler the-signal-handler.^)
  (the-signal-handler .^= (on (@evil)
                            (the-signal-handler .^= parent-handler)
                            (handler @evil)))
  (let result (thunk))
  (the-signal-handler .^= parent-handler)
  result)

(to (unwind-protect try finally)     ;TODO better name
  (let parent-handler the-signal-handler.^)
  (the-signal-handler .^= (to (unwind-protector @evil)
                            (the-signal-handler .^= parent-handler)
                            (finally)
                            (parent-handler @evil)))
  (let result (try))
  (the-signal-handler .^= parent-handler)
  (finally)
  result)

(to (fallback-signal-handler @evil)
  ;; XXX for some reason, the panic handler doesn't seem to be set
  ;; when we're in here. So, for now let's just set it:
  (the-signal-handler .^= panic)

  ;; N.B. we're trying to use only primitives here as far as possible:
  (display "Error within error! Evils:\n")
  (begin printing ((xs evil))
    (when xs.empty? (os-exit 1))
    (out .write xs.first)
    (out .display #\newline)
    (printing xs.rest)))

(to (breakpoint @values)
  (error @`("Breakpoint" ,@values)))

(to (system/must-succeed command)
  (unless (= 0 (system command))
    (error "Failed system command" command)))

(to (listener @(optional cmd-line-args))    ;TODO rename
  (import (use 'traceback) on-error-traceback)

  (let parent-handler the-signal-handler.^)

  (to (script-handler @evil)
    (the-signal-handler .^= parent-handler)
    (the-last-error .^= evil)
    (on-error-traceback @evil))

  (to (listener-handler @evil)
    (the-signal-handler .^= parent-handler)
    (the-last-error .^= evil)
    (on-error-traceback @evil)
    (display "Enter (debug) for more.\n")
    (interact))

  (to (interact)
    (the-signal-handler .^= listener-handler)
    (display "-> ")
    (may (read)
      (be (? eof?) (newline))
      (be sexpr (print-result (evaluate sexpr '()))))) ;XXX reify a proper env object

  ;; A separate function just to make the top of tracebacks cleaner.
  (to (print-result value)
    (unless (= value void)
      (print value))
    (interact))

  (may cmd-line-args
    (be #no (interact))
    (be '() (interact))
    (be `("-i" ,filename ,@_)
      (the-signal-handler .^= listener-handler)
      (load-and-run filename cmd-line-args.rest)
      (interact))
    (be `(,filename ,@_)
      (the-signal-handler .^= script-handler)
      (load-and-run filename cmd-line-args))))

(to (load-and-run filename args)
  (load filename `(,filename)) ;TODO remove .scm extension
  (when (global-defined? 'main)     ;XXX hack
    ((evaluate 'main '()) args)))

(to (debug)
  (import (use 'debugger) inspect-continuation)
  (may the-last-error.^
    (be `(,k ,@evil)
      (inspect-continuation k))
    (else
      (display "No error to debug.\n"))))

(let the-modules (map<-))

;; To make it possible to reload a module by calling (use file-stem)
;; again afterward. N.B. that won't mutate the existing module object.
(to (unuse file-stem)                   ;TODO better name
  (the-modules .delete! file-stem))

(to (use file-stem)                  ;TODO a realer module system
  ;; N.B. could sort of just use memoize if that were already loaded.
  (let stem (if (symbol? file-stem)
                (chain "library/" file-stem.name)
                file-stem))
  (may (the-modules .get stem)
    (be #no
      (let mod (load-module (chain stem ".scm") `(,stem)))
      (the-modules .set! stem mod)
      mod)
    (be mod
      mod)))

(to (load filename @(optional context))
  (load-exp `(do ,@(with-input-file read-all filename))
            context))

(to (load-module filename @(optional context))
  (load-exp `(hide ,@(with-input-file read-all filename))
            context))

(to (load-exp exp context)
  (let code (parse-exp exp (or context '())))
  (evaluate code '()))

(to (with-input-file fn filename)
  (let source (open-input-file filename))
  (let result (fn source))
  source.close                       ;TODO unwind-protect
  result)

(to (with-output-file fn filename)
  (let sink (open-output-file filename 'replace)) ;TODO the 'replace is for Chez
  (let result (fn sink))
  sink.close                       ;TODO unwind-protect
  result)

(to (read-all source) ;XXX confusing name, since source.read-all returns a string
  (let thing (read source))
  (if (eof? thing)
      '()
      (link thing (read-all source))))

(the-signal-handler .^= fallback-signal-handler)
