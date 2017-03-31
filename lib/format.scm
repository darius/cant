;; printf-ish thing. TODO do something completely different?

(make format
  ((format-string @arguments)
   (really-format out format-string arguments))
  ({.to sink format-string @arguments}
   (really-format sink format-string arguments)))

;;TODO actually design the format language
(to (really-format sink format-string arguments)
  (begin scanning ((s format-string)
                   (args arguments))
    (if s.empty? 
        (unless args.empty?
          (error "Leftover arguments" args))
        (match s.first
          (#\~
           (begin parsing ((s s.rest) (width #no))
             (when s.empty?
               (error "Incomplete format" format-string))
             (match s.first
               (#\w
                (maybe-pad sink {.print args.first} width)
                (scanning s.rest args.rest))
               (#\d
                (maybe-pad sink {.display args.first} width)
                (scanning s.rest args.rest))
               (#\~
                (sink .display "~")
                (scanning s.rest args))
               ((: ch '.digit?)
                (let digit (- ch.code 48))
                (parsing s.rest (+ (if width (* 10 width) 0)
                                   digit)))
               (_
                (error "Bad format string" s)))))
          (ch
           (sink .display ch)
           (scanning s.rest args))))))

(to (maybe-pad sink message width)
  (if width
      (sink .display ((with-output-string (given (o) (call o message)))
                      .right-justify width)) ;TODO left/right
      (call sink message)))

(export format)
