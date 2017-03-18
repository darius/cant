;; To check how sturm interprets your keystrokes.
;; We quit on Q instead of the escape key because there are still
;; unmapped keys with escape sequences.

(import (use "lib/sturm")
  cbreak-mode get-key render cursor screen-width)
(import (use "lib/text-wrap")
  fill)

(to (main _)
  (cbreak-mode run))

(to (run)
  (let strokes (fillvector<-))
  (begin running ()
    (let echoes (fill (" " .join strokes) screen-width))
    (render `("Hit some keys; or hit capital Q to quit.\n\n"
              ,echoes ,cursor))
    (let key (get-key))
    (unless (= key #\Q)
      (strokes .push! ("~w" .format key))
      (running))))

(export main run)
