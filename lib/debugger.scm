(import (use "lib/traceback") print-traceback)

(to (inspect-cont k)
  (surely (not k.empty?))

  (to (help)
    (for each! (((short full text) vocab))
      (format "%d %d - %d\n" ;XXX format should be able to do the padding
              short (pad-right full.name 8) text))
    (say ""))

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
       (continue "Huh? Enter `help` for help.")))))

(to (show-env env)
  (print (each '.first env)))

(to (pad-right str n)                  ;XXX make this a string method?
   (let pad (- n str.count))
   (if (<= pad 0)
       str
       (chain str (" " .repeat pad))))

(to (say message)
  (display message)
  (newline))

(to (main _)
  (to (on-error k plaint @values)
    (the-signal-handler-box .^= default-handler)
    (print-error plaint values)
    (inspect-cont k))
  (to (print-error plaint values)
    (display "Error! ")
    (display plaint)
    (display ": ")
    (write values)
    (newline))
  (let default-handler the-signal-handler-box.^)
  (the-signal-handler-box .^= on-error)
  (factorial 5))

(to (factorial n)
  (if (= n 0) oops (* n (factorial (- n 1)))))
