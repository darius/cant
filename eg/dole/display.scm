;; Update what's shown on the screen.

(import (use "lib/ansi-term") 
  cursor-hide cursor-show home clear-to-right goto)
(import (use "eg/dole/console")
  logs)

;; The screen size.
(let `(,rows ,cols) '(18 80))              ;XXX
(let console-rows 5) ; #rows extra, reserved for the debugging console

;; An array of strings, one per screen row.
(to (display-buffer<-)
  (array<-list ('("") .repeat rows)))

;; Track what's on the screen, to avoid redundant writes.
(let showing (display-buffer<-))

;; Return a string of glyphs representing character ch at column x.
(to (render-glyph ch x)
   (let b ch.code)
   (if (and (<= 32 b) (< b 127))  ;; TODO char.printable?
       (string<- ch)
       ("\\x~02x" .format b)))

;; Compute how to show `text` from coordinate `start` with cursor at
;; `point`. Return an object that can say whether the cursor is visible
;; and can show the rendering.
(to (render text start point)
  (let lines (display-buffer<-))
  (let point-x (box<- #no))
  (let point-y (box<- #no))
  (begin rendering ((p start) (x 0) (y 0))
    (when (< y rows)
      (when (= p point)
        (point-x .^= x)
        (point-y .^= y))
      (let ch (text .get p 1))
      (let pp (+ p 1))
      (case ((or (= ch "") (= ch "\n"))
             ;; TODO flexarray to string, i guess
             (rendering pp 0 (+ y 1)))
            (else
             (begin appending ((glyphs (render-glyph ch.first x))
                               (x x)
                               (y y))
               (case (glyphs.empty? (rendering pp x y))
                     (else
                      (lines .set! y (chain (lines y) (string<- glyphs.first))) ;XXX quadratic
                      (case ((< (+ x 1) cols) (appending glyphs.rest (+ x 1) y))
                            ((< (+ y 1) rows) (appending glyphs.rest 0 (+ y 1)))
                            (else 'done)))))))))
  (make _
    ({.point-visible?}  (not (not point-y.^)))
    ;; XXX doesn't mean 'is centered' any more:
    ({.point-centered?} (and point-y.^ (<= point-y.^ (rows .quotient 2))))
    ({.show}
     (display cursor-hide)
     (display home)
     (for each! ((`(,i ,line) lines.items))
       (unless (= line (showing i))
         (display (goto 0 i))
         (display line)
         (display clear-to-right)
         (showing .set! i line)))
     ;; TODO: styling for the logs
     (display (goto 0 rows))
     (for each! ((message (logs.^ .slice 0 console-rows)))
       (display (message .slice 0 cols))
       (display clear-to-right)
       (display "\r\n"))
     (display (goto point-x.^ point-y.^))
     (display cursor-show))))

(export
  cols rows
  render)
