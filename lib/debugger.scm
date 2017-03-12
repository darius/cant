(import (use "lib/traceback") print-traceback)

(to (inspect-cont k)
  (surely (not k.empty?))

  (to (help)
    (for each! (((short full text) vocab))
      (format "~d ~d - ~d\n" ;XXX format should be able to do the justify
              short (full.name .left-justify 9) text)))

  (let vocab
    '((? help      "this message")
      (q quit      "quit the debugger")
      (u up        "up to caller")
      (d down      "down to callee")
      (e env       "enumerate the variables in the current environment")
      (v value     "evaluate an expression in the current environment")
      (b backtrace "show all of the stack up from here")))

  (let abbrevs
    (map<-a-list (for each (((short full _) vocab))
                   `(,short ,full))))

  (say "Enter ? for help.")
  (begin interacting ((frame k) (callees '()))

    (to (continue @messages)
      (each! say messages)
      (interacting frame callees))

    (display "debug> ")
    (let input (read))
    (match (abbrevs .get input input)
      ('help
       (help)
       (continue))
      ('quit
       'ok)
      ('up
       (let caller frame.rest)
       (if caller.empty?
           (continue "At top.")
           (interacting caller (cons frame callees)))) ;TODO show the new current frame
      ('down
       (if callees.empty?
           (continue "At bottom.")
           (interacting callees.first callees.rest)))
      ('env
       (show-env frame.env)
       (continue))
      ('value
       (print (evaluate (parse-exp (read)) frame.env))
       (continue))
      ('backtrace
       (print-traceback frame)
       (continue))
      (_
       (unless (eof-object? input)
         (continue "Huh? Enter 'help' for help."))))))

(to (say message)
  (display message)
  (newline))

(to (show-env env)
  (print (each '.first env)))

(export inspect-cont)
