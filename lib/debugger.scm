;; Interactively explore the call stack or an arbitrary object.

(import (use 'traceback) print-traceback)

(to (help vocab)
  (for each! ((`(,short ,full ,text) vocab))
    (format "~d ~-9w - ~d\n"
            short full text)))

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
      (interacting new-focus (link focus trail)))

    (display "inspect-> ")
    (let input (read))
    (may (abbrevs .get input input)
      (be 'help
        (help vocab)
        (format "<n>         - inspect the nth component of the focus\n")
        (continue))
      (be 'quit
        'ok)
      (be (? eof?)
       'ok)
      (be 'up
        (if trail.empty?
            (continue "At top.\n")
            (interacting trail.first trail.rest)))
      (be 'top
        (interacting initial-focus '()))
      (be 'script
        (push (extract-script focus)))
      (be 'datum
        (push (extract-datum focus)))
      (be 'value
        (let focus-env '())              ;XXX
        (let env `((*focus* ,focus) ,@focus-env))
        (print (evaluate (read) env))
        (continue))
      (be (? count?)
        (push (focus input))) ;TODO handle out-of-range
      (else
        (continue "Huh? Enter 'help' for help.")))))

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
      (evaluate (read) frame.env))

    (display "debug-> ")
    (let input (read))  ; TODO read a line, then parse?
    (may (abbrevs .get input input)
      (be 'help
        (help vocab)
        (continue))
      (be 'quit
        'ok)
      (be (? eof?)
        'ok)
      (be 'resume
        (frame .answer (read-eval)))
      (be 'up
        (let caller frame.rest)
        (if caller.empty?
            (continue "At top.\n")
            (interacting caller (link frame callees)))) ;TODO show the new current frame
      (be 'down
        (if callees.empty?
            (continue "At bottom.\n")
            (interacting callees.first callees.rest)))
      (be 'env
        (show-env frame.env)
        (continue))
      (be 'value
        (print (read-eval))
        (continue))
      (be 'backtrace
        (print-traceback frame)
        (continue))
      (else
        (continue "Huh? Enter 'help' for help.\n")))))

(to (show-env env)
  (print (each _.first env)))

(export inspect inspect-continuation)
