;; The top-level text editor

(import (use 'sturm)                     raw-mode)
(import (use "eg/dole/keyboard")         read-key)
(import (use "eg/dole/fundamental-mode") fundamental-mode<-)

(to (dole ?filename)
   (let buffer (fundamental-mode<-))
   (when ?filename
     (buffer .visit ?filename))
   (for raw-mode ()
     (edit buffer))
   (newline))

(to (edit buffer)
  (begin editing ()
    (buffer .redisplay)
    (let key (read-key))
    (let command (buffer.key-map key))
    (unless (= command 'exit)
      (command key)
      (editing))))
   
(export dole edit)
