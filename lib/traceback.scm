;; An error handler that prints a (crude) traceback.

;; We try to call on less of the library than usual, to work better
;; when the system is borked.

(import (use "lib/cycle-write") cycle-write)

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

(to (write-to-bounded-string frame width)
  ;; TODO custom sink type, using an ejector to quit early, don't bother about cycles
  (let s (with-output-string (given (sink)
                               (cycle-write frame sink))))
  ;; TODO make it unambiguous whether it's truncated, somehow
  (if (< s.count width)
      s
      (chain (s .slice 0 (- width 2)) "..")))

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
