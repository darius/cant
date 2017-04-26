(let the-signal-handler (box<- panic))
(let the-last-error (box<- #no))

(to (push-signal-handler handler)
  (let parent-handler the-signal-handler.^)
  (to (handler/wrapped @evil)
    ;; TODO might be cleaner with unwind-protect actions spelled out inline
    (unwind-protect
     (to (handler-thunk)
       (the-signal-handler .^= parent-handler)
       (call handler evil))
     (to (unwind-thunk)
       (the-signal-handler .^= handler/wrapped))))
  (the-signal-handler .^= handler/wrapped))

(to (with-signal-handler handler thunk)
  (let parent-handler the-signal-handler.^)
  (the-signal-handler .^= (given (@evil)
                            (the-signal-handler .^= parent-handler)
                            (call handler evil)))
  (let result (thunk))
  (the-signal-handler .^= parent-handler)
  result)

(to (unwind-protect try finally)     ;TODO better name
  (let parent-handler the-signal-handler.^)
  (the-signal-handler .^= (to (unwind-protector @evil)
                            (the-signal-handler .^= parent-handler)
                            (finally)
                            (call parent-handler evil)))
  (let result (try))
  (the-signal-handler .^= parent-handler)
  (finally)
  result)

(to (fallback-signal-handler @evil)
  ;; XXX for some reason, the panic handler doesn't seem to be set
  ;; when we're in here. So, for now let's just set it:
  (the-signal-handler .^= panic)

  ;; N.B. we're trying not to use anything but primitives:
  (display "Error within error! Evils:\n")
  (to (printing xs)
    (unless xs.empty?
      (out .print xs.first)
      (out .display #\newline)
      (printing xs.rest)))
  (printing evil)
  (os-exit 1))

(to (break @values)
  (call error `("Breakpoint" ,@values)))

(to (system/must-succeed command)
  (unless (= 0 (system command))
    (error "Failed system command" command)))

(to (repl @(optional cmd-line-args))    ;TODO rename
  (import (use "lib/traceback") on-error-traceback)

  (let parent-handler the-signal-handler.^)
  (to (repl-handler @evil)
    (the-signal-handler .^= parent-handler)
    (the-last-error .^= evil)
    (call on-error-traceback evil)
    (display "Enter (debug) for more.\n")
    (interacting))

  (to (interacting)
    (the-signal-handler .^= repl-handler)
    (display "sqm> ")
    (let sexpr (read))
    (unless (eof? sexpr)
      (let e (parse-exp sexpr))
      (let value (evaluate e '())) ;XXX reify a proper env object
      (unless (= value void)
        (print value))
      (interacting))
    (newline))

  (match cmd-line-args
    (#no (interacting))
    ('() (interacting))
    (`(,filename ,@_)
     (the-signal-handler .^= repl-handler)     
     (load-and-run filename cmd-line-args)
     (interacting))))

(to (load-and-run filename args)
  (load filename `(,filename)) ;TODO remove .scm extension
  (when ((list-globals) .find? 'main)     ;XXX hack
    ((evaluate (parse-exp 'main '()) '()) args)))

(to (debug)
  (import (use "lib/debugger") inspect-continuation)
  (match the-last-error.^
    (`(,k ,@evil) (inspect-continuation k))
    (_ (display "No error to debug.\n"))))

(let the-modules (box<- '()))

;; To make it possible to reload a module by calling (use file-stem)
;; again afterward. N.B. that won't mutate the existing module object.
;; This is not very useful, though, because we still can't redefine
;; variables at the repl.
(to (unuse file-stem)                   ;TODO better name
  (the-modules .^= (for those ((`(,stem ,mod) the-modules.^))
                     (not= stem file-stem))))

(to (use file-stem)                  ;TODO a realer module system
  ;; N.B. could sort of just use memoize if that were already loaded.
  (match (assoc file-stem the-modules.^)
    (`(,_ ,mod) mod)
    (#no
     (let mod (load-module (chain file-stem ".scm") `(,file-stem)))
     (the-modules .^= `((,file-stem ,mod) ,@the-modules.^))
     mod)))

(to (load filename @(optional context-arg))
  ;; XXX duplication
  (let context (or context-arg '()))
  (let code `(do ,@(with-input-file read-all filename)))
  (let code1 (parse-exp code context))
  (evaluate code1 '()))

(to (load-module filename @(optional context-arg))
  (let context (or context-arg '()))
  (let code `(hide ,@(with-input-file read-all filename)))
  (let code1 (parse-exp code context))
  (evaluate code1 '()))

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

(to (read-all source)
  (let thing (read source))
  (if (eof? thing)
      '()
      (cons thing (read-all source))))

(the-signal-handler .^= fallback-signal-handler)
