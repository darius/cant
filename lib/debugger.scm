;; Interactively introspect into the call stack or an arbitrary object.

(import (use "lib/traceback") print-traceback)

(to (help vocab)
  (for each! ((`(,short ,full ,text) vocab))
    (format "~d ~-9d - ~d\n"
            short full.name text)))

(to (collect-abbrevs vocab)
  (map<- (for each ((`(,short ,full ,_) vocab))
           `(,short ,full))))

;; TODO unify with inspect-continuation
(to (inspect initial-focus)
  (let vocab
    '((? help      "this message")
      (q quit      "quit the inspector")
      (u up        "up to parent")
      (t top       "up all the way to the original focus")
      (s script    "inspect the script of the focus")
      (d datum     "inspect the datum of the focus")
      (v value     "evaluate an expression in the focus's env (+ *focus*)")))
  (let abbrevs (collect-abbrevs vocab))

  (display "Enter ? for help.\n")
  (begin interacting ((focus initial-focus) (trail '()))

    (to (continue @messages)
      (each! display messages)
      (interacting focus trail))

    (to (push new-focus)
      (print new-focus)           ;XXX pretty-print? cycle-print? ...?
      (interacting new-focus (cons focus trail)))

    (display "inspect> ")
    (let input (read))
    (match (abbrevs .get input input)
      ('help
       (help vocab)
       (format "<n>         - inspect the nth component of the focus\n")
       (continue))
      ('quit
       'ok)
      ('up
       (if trail.empty?
           (continue "At top.\n")
           (interacting trail.first trail.rest)))
      ('top
       (interacting initial-focus '()))
      ('script
       (push (extract-script focus)))
      ('datum
       (push (extract-datum focus)))
      ('value
       (let focus-env '())              ;XXX
       (let env `((*focus* ,focus) ,@focus-env))
       (print (evaluate (parse-exp (read)) env))
       (continue))
      (_
       (case ((eof? input) 'ok)
             ((integer? input) (push (focus input))) ;TODO handle negative
             (else (continue "Huh? Enter 'help' for help.")))))))

(to (inspect-continuation k)
  (surely (not k.empty?))               ;XXX require

  (let vocab
    '((? help      "this message")
      (q quit      "quit the debugger")
      (r resume    "continue from here, with the value of an expression")
      (u up        "up to caller")
      (d down      "down to callee")
      (e env       "enumerate the variables in the current environment")
      (v value     "evaluate an expression in the current environment")
      (b backtrace "show all of the stack up from here")))
  (let abbrevs (collect-abbrevs vocab))

  (display "Enter ? for help.\n")
  (begin interacting ((frame k) (callees '()))

    (to (continue @messages)
      (each! display messages)
      (interacting frame callees))

    (to (read-eval)
      (evaluate (parse-exp (read)) frame.env))

    (display "debug> ")
    (let input (read))
    (match (abbrevs .get input input)
      ('help
       (help vocab)
       (continue))
      ('quit
       'ok)
      ('resume
       (frame .answer (read-eval)))
      ('up
       (let caller frame.rest)
       (if caller.empty?
           (continue "At top.\n")
           (interacting caller (cons frame callees)))) ;TODO show the new current frame
      ('down
       (if callees.empty?
           (continue "At bottom.\n")
           (interacting callees.first callees.rest)))
      ('env
       (show-env frame.env)
       (continue))
      ('value
       (print (read-eval))
       (continue))
      ('backtrace
       (print-traceback frame)
       (continue))
      (_
       (unless (eof? input)
         (continue "Huh? Enter 'help' for help.\n"))))))

(to (show-env env)
  (print (each '.first env)))

(export inspect inspect-continuation)
