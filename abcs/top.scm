;; Top level: listener and command line

(let the-signal-handler (box<- panic))
(let the-last-error (box<- #no))

(to (push-signal-handler handler)
  (let parent-handler the-signal-handler.^)
  (to (handler/wrapped k evil)
    ;; TODO might be cleaner with unwind-protect actions spelled out inline
    (unwind-protect
     (to (handler-thunk)
       (the-signal-handler .^= parent-handler)
       (handler k evil))
     (to (unwind-thunk)
       (the-signal-handler .^= handler/wrapped))))
  (the-signal-handler .^= handler/wrapped))

(to (with-signal-handler handler thunk)
  (let parent-handler the-signal-handler.^)
  (the-signal-handler .^= (on (k evil)
                            (the-signal-handler .^= parent-handler)
                            (handler k evil)))
  (let result (thunk))
  (the-signal-handler .^= parent-handler)
  result)

(to (unwind-protect try finally)     ;TODO better name
  (let parent-handler the-signal-handler.^)
  (the-signal-handler .^= (to (unwind-protector k evil)
                            (the-signal-handler .^= parent-handler)
                            (finally)
                            (parent-handler k evil)))
  (let result (try))
  (the-signal-handler .^= parent-handler)
  (finally)
  result)

(to (fallback-signal-handler k evil)
  ;; XXX for some reason, the panic handler doesn't seem to be set
  ;; when we're in here. So, for now let's just set it:
  (the-signal-handler .^= panic)

  ;; N.B. we're trying to use only primitives here as far as possible:
  (display "Error within error! Evils:\n")
  (begin printing ((xs evil))
    (when xs.none? (os-exit 1))
    (out .write xs.first)
    (out .display #\newline)
    (printing xs.rest)))

(to (breakpoint @values)
  ;; TODO automatically invoke the debugger
  (error "Breakpoint" @values))

(to (system/must-succeed command)
  (unless (= 0 (system command))
    (error "Failed system command" command)))

(to (listener @(optional cmd-line-args))    ;TODO rename
  (import (use 'traceback) on-error-traceback)

  (let parent-handler the-signal-handler.^)

  (to (script-handler k evil)
    (the-signal-handler .^= parent-handler)
    (the-last-error .^= {error k evil})
    (on-error-traceback k evil))

  (to (listener-handler k evil)
    (the-signal-handler .^= parent-handler)
    (the-last-error .^= {error k evil})
    (on-error-traceback k evil)
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
    (be {error k evil}
      (inspect-continuation k))
    (else
      (display "No error to debug.\n"))))

(the-signal-handler .^= fallback-signal-handler)
