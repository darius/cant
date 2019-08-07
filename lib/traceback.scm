;; An error handler that prints a (crude) traceback.

;; We try to call on less of the library than usual, to work better
;; when the system is borked.

(to (on-error-traceback k @evil)
  (display "Error! Traceback:\n")
  (print-traceback k)
  (complain @evil))

(to (on-error-complain k @evil)
  (display "Error!\n")
  (complain @evil))

(to (print-traceback k)
  (for each! ((frame (reverse k)))
    (format "  ~d\n" (write-to-bounded-string frame 77))))

(to (complain @evil)
  (may evil
    (be `(,(? string? plaint) ,@values)
      (display plaint)
      (display ": ")
      (display (write-to-bounded-string values (* 80 20))))
    (else
      (display "Nonstandard evil: ")
      (write evil)))
  (newline))

;; Write `thing` into a string, but give up and truncate if it
;; overflows `width`.
(to (write-to-bounded-string thing width)
  (let buffer (flexarray<-))
  (let total (box<- 0)) ;; Kept equal to (sum _.count buffer)
  (to (output)
    (let s ("" .join buffer.values))
    ;; TODO make it unambiguous whether it's truncated, somehow
    (if (< total.^ width)
        s
        (chain (s .slice 0 (- width 2)) "..")))
  (with-ejector
   (on (ejector)
     (let ss (string-sink<-))
     (to (cut-off)
       (buffer .push! ss.output-string) ;TODO worth checking to skip if output-string empty?
       (total .^= (+ total.^ buffer.last.count))
       (when (<= width total.^)
         (ejector .eject (output))))
     (make bounded-sink
       (to (_ .display a)   (ss .display a)   (cut-off))
       (to (_ .write-u8 u8) (ss .write-u8 u8) (cut-off))
       (to (_ .print a)     (a .selfie bounded-sink))
       (to _.close          ss.close))
     (bounded-sink .print thing)
     (output))))
       
(export on-error-traceback on-error-complain complain print-traceback)
