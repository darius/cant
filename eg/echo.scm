;; To check how sturm interprets your keystrokes.
;; We quit on Q instead of the escape key because there are still
;; unmapped keys with escape sequences.

(import (use "lib/sturm")
  cbreak-mode get-key render cursor)

(define (main _)
  (cbreak-mode run))

(define (run)
  (let strokes (fillvector<-))
  (begin running ()
    (let lines (text-wrap (" " .join strokes)     ;XXX does this work
                          num-cols))
    (render '("Hit some keys; or hit capital Q to quit.\n\n"
              ,("\n" .join lines)
              ,cursor))
    (let key (get-key))
    (unless (= key #\Q)
      (strokes .push! ("%w" .format key))
      (running))))

;;XXX stubs
(let num-cols 80)
(define (text-wrap str width)
  `(,str))      

(export main run)
