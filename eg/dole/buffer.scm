;; (Ported from github.com/darius/dole)
;; A buffer is a text with a current point of editing, a display, and
;; a key-map.

(import (use "lib/text")         text<- backward forward)
(import (use "eg/dole/char-set") char-set<-)
(import (use "eg/dole/display")  render rows cols)   ; XXX rename to num-rows or something
(import (use "eg/dole/key-map")  key-map<-)
(import (use "eg/dole/console")  log)

;; Return the smallest i in [lo..hi) where ok(i), if any; else hi.
;; Pre: lo and hi are ints, lo < hi
;; and not ok(j) for j in [lo..i)
;; and     ok(j) for j in [i..hi)  (for some i in [lo..hi]).
(to (search lo hi ok?)
  (if (ok? lo)
      lo
      (begin searching ((L lo) (H hi))
        (if (<= H (+ L 1))
            H
            (do (let mid ((+ L H) .quotient 2))
                (if (ok? mid)
                    (searching L mid)
                    (searching mid H)))))))

(let newline (char-set<- #\newline))

;; Return a new buffer.
(to (buffer<-)

  (let text (text<-))
  (let point (box<- 0))              ; TODO: make this a mark
  (let origin (box<- 0))     ; display origin. XXX keep in a window object?

  (to (update-origin)
    (let rendering (render text origin.^ point.^))
    (case (rendering.point-visible? rendering)
          (else
           (to (has-point? o)
             ((render text o point.^) .point-centered?))
           (let screen-size (* rows cols))
           (origin .^= (search (text .clip (- point.^ screen-size))
                               point.^
                               has-point?))
           (when (= origin.^ point.^)
             (origin .^= 0)) ; Couldn't center it.
           (render text origin.^ point.^))))

  (to (insert str)
    (text .insert point.^ str)
    (point .^= (+ point.^ str.count)))

  (to (find-line p dir)
    (text .clip (text .find-char-set p dir newline)))

  ;; TODO: preserve goal column; respect formatting, such as tabs;
  ;; treat long lines as defined by display
  (to (previous-line)
    (let start      (find-line point.^ backward))
    (let offset     (- point.^ start))
    (let prev-start (find-line (- start 1) backward))
    (point .^= (min (+ prev-start offset)
                    (text .clip (- start 1)))))

  (to (next-line)
    (let start      (find-line point.^ backward))
    (let offset     (- point.^ start))
    (let next-start (find-line start forward)) ;XXX why not start from point?
    (let next-end   (find-line next-start forward))
    (point .^= (min (+ next-start offset)
                    (text .clip (- next-end 1)))))
  ;; XXX this can wrap around since text .clip moves `nowhere` to 0.
   
  (let key-map (key-map<- (given (ch) (insert (string<- ch)))))

  (make buffer

    ({.clear}
     text.clear
     (point .^= 0)
     (origin .^= 0))

    ({.backward-delete-char}
     (text .delete (- point.^ 1) 1)
     (buffer .move-char backward))

    ({.forward-delete-char}
     (text .delete point.^ 1))

    ({.beginning-of-line}
     (point .^= (find-line point.^ backward)))

    ({.end-of-line}
     (point .^= (text .clip (- (find-line point.^ forward) 1))))

    ({.insert str}
     (surely (string? str))
     (insert str))

    ({.key-map}
     key-map)

    ({.move-char offset}
     (point .^= (text .clip (+ point.^ offset))))

    ({.previous-line} (previous-line))
    ({.next-line}     (next-line))

    ;; TODO: more reasonable/emacsy behavior. This interacts quite badly
    ;; with the dumb update-origin() logic.
    ({.previous-page}
     ;; (update-origin)
     ;; point .^= origin
     (each! previous-line (range<- rows)))

    ({.next-page}
     ;; (update-origin)
     ;; point .^= origin
     (each! next-line (range<- rows)))

    ({.redisplay}
     (let rendering (update-origin))
     (surely rendering.point-visible?)
     rendering.show)

    ({.visit filename}
     buffer.clear
     (let contents (with-input-file read-into-string filename))
     (text .insert 0 contents))))

(export
  buffer<-)
