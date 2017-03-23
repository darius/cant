;; Hi, I'd like a buffer with all the usuals, please.

(import (use "eg/dole/buffer")  buffer<-)
(import (use "eg/dole/key-map") ctrl)

(let C ctrl)

(to (fundamental-mode<-)
  (let buffer (buffer<-))

   (to (backward-char _) (buffer .move-char -1))
   (to (forward-char _)  (buffer .move-char  1))

   (let bindings
     `((,(C #\B)  ,backward-char)
       (left      ,backward-char)
       (,(C #\F)  ,forward-char)
       (right     ,forward-char)
       (,(C #\N)  ,(given (_) (buffer .next-line)))
       (down      ,(given (_) (buffer .next-line)))
       (,(C #\P)  ,(given (_) (buffer .previous-line)))
       (up        ,(given (_) (buffer .previous-line)))
       (,(C #\Q)  exit)
       (,(C #\M)  ,(given (_) (buffer .insert "\n")))
       (backspace ,(given (_) (buffer .backward-delete-char)))
       (del       ,(given (_) (buffer .forward-delete-char)))
       (end       ,(given (_) (buffer .end-of-line)))
       (home      ,(given (_) (buffer .beginning-of-line)))
       (pgup      ,(given (_) (buffer .previous-page)))
       (pgdn      ,(given (_) (buffer .next-page)))))

   (for each! (((key command) bindings)) ;TODO should be a key-map method
     (buffer.key-map .set! key command))
      
   buffer)

(export fundamental-mode<-)
