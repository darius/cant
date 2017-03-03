;; (Ported from github.com/darius/dole)
;; A buffer is a text with a current point of editing, a display, and
;; a keymap.

(import (use "charset.scm") charset<-)
(import (use "display.scm") render rows cols)   ; XXX rename to num-rows or something
(import (use "keymap.scm")  keymap<-)
(import (use "text.scm")    text<-)

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

;; Return a new buffer.
(define (buffer<-)

   (let text (text<-))
   (let point (box<- 0))              ; TODO: make this a mark
   (let origin (box<- 0))     ; display origin. XXX keep in a window object?

   (define (clear)
      text.clear
      (point .^= 0))

   (define (visit filename)
     (let contents (with-input-file read-into-string filename))
     (text .insert 0 contents))

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
            (let rendering (render text origin.^ point.^))
            (case ((= origin.^ point.^)
                   (origin .^= 0) ; Couldn't center it.
                   (render text origin.^ point.^))
                  (else rendering)))))

   (define (redisplay)
     (let rendering (update-origin))
     (assert rendering.point-is-visible?)
     rendering.show)

   (define (insert ch)                  ;XXX change to expect char instead of string?
     (text.insert point.^ ch)
     (point .^= (+ point.^ ch.size)))

   (define (move-char offset)
     (point .^= (text.clip (+ point.^ offset))))

   (define (backward-delete-char)
     (text.delete (- point.^ 1) 1)
     (move-char -1))

   (define (forward-delete-char)
     (text.delete point.^ 1))

   (let newline (charset<- #\newline))

   (define (find-line p dir)
     (text.clip (text.find-char-set p dir newline)))

   (define (beginning-of-line)
     (point .^= (find-line point.^ -1)))

   (define (end-of-line)
     (point .^= (text.clip (- (find-line point.^ 1) 1))))

   ;; TODO: preserve goal column; respect formatting, such as tabs;
   ;; treat long lines as defined by display
   (define (previous-line)
     (let start      (find-line point.^ -1))
     (let offset     (- point.^ start))
     (let prev-start (find-line (- start 1) -1))
     (point .^= (min (+ prev-start offset)
                     (text.clip (- start 1)))))

   (define (next-line)
     (let start      (find-line point.^ -1))
     (let offset     (- point.^ start))
     (let next-start (find-line start 1))
     (let next-end   (find-line next-start 1))
     (point .^= (min (+ next-start offset)
                     (text.clip (- next-end 1)))))
   ;; XXX this can wrap around since text.clip moves `nowhere` to 0.
   
   ;; TODO: more reasonable/emacsy behavior. This interacts quite badly
   ;; with the dumb update-origin() logic.
   (define (previous-page)
     ;; (update-origin)
     ;; point .^= origin
     (each! previous-line (range<- rows)))

   (define (next-page)
     ;; (update-origin)
     ;; point .^= origin
     (each! next-line (range<- rows)))

   (let keymap (keymap<- insert))

   (export 
     backward-delete-char
     beginning-of-line
     end-of-line
     forward-delete-char
     insert
     keymap
     move-char
     next-line
     previous-line
     next-page
     previous-page
     redisplay
     visit))

(export
  buffer<-)
