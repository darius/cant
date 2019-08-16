;; Update what's shown on the screen.

(import (use 'ansi-term) 
  cursor-hide cursor-show home clear-to-right goto)
(import (use "eg/dole/console")
  logs)

;; The screen size.
(let (_ rows cols) (_ 18 80))          ;TODO query for it or something
(let console-rows 5) ; #rows extra, reserved for the debugging console

;; An array of strings, one per screen row.
(to (display-buffer<-)
  (array<-list ('("") .repeat rows)))

;; Track what's on the screen, to avoid redundant writes.
(let showing (display-buffer<-))

;; Return a string of glyphs representing character ch at column x.
(to (render-char ch x)
  (if ch.printable?
      (string<- ch)
      ("\\x~02x" .format ch.code)))

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
      (if (or (= ch "") (= ch "\n"))
          (rendering pp 0 y.+)  ;; TODO flexarray to string, i guess
          (begin appending ((glyphs (render-char ch.first x))
                            (x x)
                            (y y))
            (hm (if glyphs.none? (rendering pp x y))
                (do (lines .set! y
                           (chain (lines y) (string<- glyphs.first)))) ;XXX quadratic
                (if (< x.+ cols) (appending glyphs.rest x.+ y))
                (if (< y.+ rows) (appending glyphs.rest 0 y.+))
                (else            'done))))))
  (make _
    (to _.point-visible?  (yeah? point-y.^))
    ;; XXX doesn't mean 'is centered' any more:
    (to _.point-centered? (and point-y.^ (<= point-y.^ (rows .quotient 2))))
    (to _.show
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
