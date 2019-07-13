;; An error handler that prints a (crude) traceback.

;; We try to call on less of the library than usual, to work better
;; when the system is borked.

(to (on-error-traceback k @evil)
  (display "Error! Traceback:\n")
  (print-traceback k)
  (call complain evil))

(to (on-error-complain k @evil)
  (display "Error!\n")
  (call complain evil))

(to (print-traceback k)
  (for each! ((frame (reverse k)))
    (format "  ~d\n" (write-to-bounded-string frame 77))))

;; Write `thing` into a string, but give up and truncate if it
;; overflows `width`.
(to (write-to-bounded-string thing width)
  (let buffer (flexarray<-))
  (to (output)
    (let s ("" .join buffer))
    ;; TODO make it unambiguous whether it's truncated, somehow
    (if (< s.count width)
        s
        (chain (s .slice 0 (- width 2)) "..")))
  (with-ejector
   (given (ejector)
     (let ss (string-sink<-))
     (to (cut-off)
       (buffer .push! ss.output-string) ;TODO worth checking to skip if output-string empty?
       (when (<= width (sum (each '.count buffer)))
         (ejector .eject (output))))
     (make bounded-sink
       ({.display a}   (ss .display a)   (cut-off))
       ({.write-u8 u8} (ss .write-u8 u8) (cut-off))
       ({.print a}     (a .selfie bounded-sink))
       ({.close}       (ss .close)))
     (bounded-sink .print thing)
     (output))))
       
(to (complain @evil)
  (match evil
    (`(,(? string? plaint) ,@values)
     (display plaint)
     (display ": ")
     (write values))
    (_
     (display "Nonstandard evil: ")
     (write evil)))
  (newline))

(export on-error-traceback on-error-complain complain print-traceback)
