;; printf-ish thing. TODO do something completely different?

(make format
  (`(,format-string ,@arguments)
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
         (let ss s.rest)
         (if (ss .starts-with? "-")
             (parse sink ss.rest -1 #no args)
             (parse sink ss #no #no args)))
        (ch
         (sink .display ch)
         (scanning sink s.rest args)))))

(to (parse sink s sign width args)
  (if (s .starts-with? "0")
      (parsing sink s.rest #\0     sign width args)
      (parsing sink s      #\space sign width args)))

(to (parsing sink s pad sign width args)
  (when s.empty?
    (error "Incomplete format" format-string))
  (match s.first
    (#\w
     (maybe-pad sink pad sign width {.print args.first})
     (scanning sink s.rest args.rest))
    (#\d
     (maybe-pad sink pad sign width {.display args.first})
     (scanning sink s.rest args.rest))
    (#\~
     (sink .display "~")
     (scanning sink s.rest args))
    ((? '.digit? ch)
     (let digit (- ch.code 48))
     (parsing sink s.rest pad sign      ;TODO testme with a multidigit width
              (+ (if width (* 10 width) 0)
                 digit)
              args))
    (_
     (error "Bad format string" s))))

(to (maybe-pad sink pad sign width message)
  (case (width
         (sink .display ((with-output-string (given (o) (call o message)))
                         .justify (if sign (* sign width) width)
                                  pad)))
        (sign
         (error "Missing width in format string"))
        (else
         (call sink message))))

(export format)
