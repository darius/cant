;; printf-ish thing. TODO do something completely different?

(make format
  ((format-string @arguments)
   (scanning out format-string arguments))
  ({.to sink format-string @arguments}
   (scanning sink format-string arguments)))

;;TODO actually design the format language

(to (scanning sink s args)
  (if s.empty? 
      (unless args.empty?
        (error "Leftover arguments" args))
      (match s.first
        (#\~
         (parsing sink s.rest #no args))
        (ch
         (sink .display ch)
         (scanning sink s.rest args)))))

(to (parsing sink s width args)
  (when s.empty?
    (error "Incomplete format" format-string))
  (match s.first
    (#\w
     (maybe-pad sink {.print args.first} width)
     (scanning sink s.rest args.rest))
    (#\d
     (maybe-pad sink {.display args.first} width)
     (scanning sink s.rest args.rest))
    (#\~
     (sink .display "~")
     (scanning sink s.rest args))
    ((: ch '.digit?)
     (let digit (- ch.code 48))
     (parsing sink
              s.rest
              (+ (if width (* 10 width) 0)
                 digit)
              args))
    (_
     (error "Bad format string" s))))

(to (maybe-pad sink message width)
  (if width
      (sink .display ((with-output-string (given (o) (call o message)))
                      .justify width)) ;TODO left/right
      (call sink message)))

(export format)
