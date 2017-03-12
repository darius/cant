(import (use "lib/traceback") print-traceback)

(to (inspect-cont k)
  (surely (not k.empty?))

  (to (help)
    (for each! (((short full text) vocab))
      (format "%d %d - %d\n" ;XXX format should be able to do the padding
              short (pad-right long 8) text))
    (say ""))

  (let vocab
    '((? help      "this message")
      (q quit      "quit the debugger")
      (u up        "up to caller")
      (d down      "down to callee")
      (e env       "show the current environment")
      (b backtrace "show all of the stack up from here")))

  (let abbrevs
    (map<-a-list (for each (((short full _) vocab))
                   `(,short ,full))))

  (begin interacting ((frame k) (callees '()))

    (to (continue @messages)
      (each! say messages)
      (interacting frame env callees))

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
           (interacting caller (cons frame callees))
      ('down
       (if callees.empty?
           (continue "At bottom.")
           (interacting callees.first callees.rest)))
      ('env
       (show-env frame.env)
       (continue))
      ('backtrace
       (print-traceback frame)
       (continue))
      (_
       (continue "Huh? Enter `help` for help.")))))
    
  
