;; (Ported from github.com/darius/dole)
;; A buffer is a text with a current point of editing, a display, and
;; a keymap.

(import (use "charset") charset<-)
(import (use "display") render rows cols)   ; XXX rename to num-rows or something
(import (use "keymap")  keymap<-)
(import (use "text")    text<- backward forward)

;; Return the smallest i in [lo..hi) where ok(i), if any; else hi.
;; Pre: lo and hi are ints, lo < hi
;; and not ok(j) for j in [lo..i)
;; and     ok(j) for j in [i..hi)  (for some i in [lo..hi]).
(define (search lo hi ok?)
  (if (ok? lo)
      lo
      (begin searching ((L lo) (H hi))
        (if (<= H (+ L 1))
            H
            (do (let mid ((+ L H) .quotient 2))
                (if (ok? mid)
                    (searching L mid)
                    (searching mid H)))))))

(let newline (charset<- #\newline))

;; Return a new buffer.
(define (buffer<-)

  (let text (text<-))
  (let point (box<- 0))              ; TODO: make this a mark
  (let origin (box<- 0))     ; display origin. XXX keep in a window object?

  (define (update-origin)
    (let rendering (render text origin.^ point.^))
    (case (rendering.point-is-visible? rendering)
          (else
           (define (has-point? o)
             ((render text o point.^) .point-is-centered?))
           (let screen-size (* rows cols))
           (origin .^= (search (text .clip (- point.^ screen-size))
                               point.^
                               has-point?))
           (when (= origin.^ point.^)
             (origin .^= 0)) ; Couldn't center it.
           (render text origin.^ point.^))))

  (define (insert ch)                  ;XXX change to expect char instead of string?
    (text.insert point.^ ch)
    (point .^= (+ point.^ ch.size)))

  (define (find-line p dir)
    (text.clip (text.find-char-set p dir newline)))

  ;; TODO: preserve goal column; respect formatting, such as tabs;
  ;; treat long lines as defined by display
  (define (previous-line)
    (let start      (find-line point.^ backward))
    (let offset     (- point.^ start))
    (let prev-start (find-line (- start 1) backward))
    (point .^= (min (+ prev-start offset)
                    (text.clip (- start 1)))))

  (define (next-line)
    (let start      (find-line point.^ backward))
    (let offset     (- point.^ start))
    (let next-start (find-line start forward))
    (let next-end   (find-line next-start forward))
    (point .^= (min (+ next-start offset)
                    (text.clip (- next-end 1)))))
  ;; XXX this can wrap around since text.clip moves `nowhere` to 0.
   
  (let keymap (keymap<- insert))

  (make buffer

    ({.clear}
     text.clear
     (point .^= 0)
     (origin .^= 0))

    ({.backward-delete-char}
     (text.delete (- point.^ 1) 1)
     (move-char backward))

    ({.forward-delete-char}
     (text.delete point.^ 1))

    ({.beginning-of-line}
     (point .^= (find-line point.^ backward)))

    ({.end-of-line}
     (point .^= (text.clip (- (find-line point.^ forward) 1))))

    ({.insert ch}
     (insert ch))

    ({.keymap}
     keymap)

    ({.move-char offset}
     (point .^= (text.clip (+ point.^ offset))))

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
     (assert rendering.point-is-visible?)
     rendering.show)

    ({.visit filename}
     buffer.clear
     (let contents (with-input-file read-into-string filename))
     (text .insert 0 contents))))

(export
  buffer<-)
