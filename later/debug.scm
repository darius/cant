;;; The interim interactive debugger
;;; XXX just a sketch

(to (debug)
  (if (continuation? @error-cont)
      (inspect-cont @error-cont)
      (@error "No context to debug")))

(to (pad-right str n)                  ;XXX make this a string method?
   (let pad (- n str.count))
   (if (<= pad 0)
       str
       (chain str (" " .repeat pad))))

(to (say message)
  (display message)
  (newline))

(to (print-each xs)
  (for each! ((x xs))
    (cycle-write x)
    (newline)))

(to (show-env env)
  (unless env.empty?
    (print-each env.inner-frame)))

(to (inspect-cont outer-frame)

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
      (e env       "show the 0th frame of the current environment")
      (n next      "show the next frame of the current environment")
      (a assembly  "show assembly source of the current procedure")
      (s stack     "show the local value stack")
      (b backtrace "names of the current procedure and its callers")))

  (let abbrevs
    (map<-a-list (for each (((short full _) vocab))
                   `(,short ,full))))

  (begin interacting ((frame   outer-frame)
                      (env     outer-frame.env)
                      (callees '()))

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
       (let caller frame.caller)
       (if caller 
           (interacting caller caller.env (cons frame callees))
           (continue "At top.")))

      ('down
       (if callees.empty?
           (continue "At bottom.")
           (interacting callees.first callees.first.env callees.rest)))

      ('env
       (show-env frame.env)
       (interacting frame frame.env callees))

      ('next
       (let next (if env.empty? env env.enclosing))
       (if next.empty? 
           (continue "No more environment frames.")
           (do (show-env next)
               (interacting frame next callees))))

      ('assembly
       (disassemble frame.code frame.pc)
       (continue))

      ('stack
       (print-each frame.stack)
       (continue))

      ('backtrace
       (print-each (for each ((frame (caller* frame)))
                     frame.code.owner))
       (continue))

      (_
       (continue "Huh? Enter HELP for help.")))))
