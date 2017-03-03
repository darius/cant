;; Ported from github.com/darius/dole

;; Glossary:
;;   p   coordinate (position between characters in text, or `nowhere`)
;;   dir direction
;;   cs  charset

;; A text is a sequence of characters. We say the characters are at
;; positions 1..size, but coordinates denote the spaces *between*
;; character positions, in [0..size]. The coordinate before the
;; first character is 0, then after the first character and before the
;; second is 1, and so on, until the coordinate after the last
;; character is `size`.
;; 
;; We store the characters at integer indices in a fillvector (starting
;; at index 0) in two chunks which we call the head and the tail,
;; separated by the gap. The fields `head` and `gap` (and `tail` if it
;; weren't implicit) denote the lengths of these spans.  `size`
;; denotes head+tail, i.e. the total length of text. The whole array
;; is of size size+gap, i.e. head+gap+tail.
;; 
;; (The gap lets us insert or delete text by moving the gap instead
;; of the whole tail; if there's locality, this will be cheaper.)


;; A coordinate that's never an actual text position. We'll use
;; -nowhere for "off the left end" and +nowhere for the right.
(let nowhere (expt 2 40))               ;XXX a dangerous pretense

;; Directions from a coordinate.
(let backward -1)
(let forward   1)

(define (text<-)

  (let t    (fillvector<-))
  (let head (box<- 0))
  (let gap  (box<- 0))
  (let size (box<- 0))

  ;; Return coordinate p clipped to the text's actual range.
  (define (clip p)
    (max 0 (min p size.^)))

  (define (clip-range p span)
    (let q (clip p))
    (let h (clip (+ q (max 0 span))))
    `(,q ,(- h q)))

  ;; Pre: p is in [0..size).
  (define (get-char-after p)
    (t (if (< p head.^) p (+ p gap.^))))

   ;; Return the `span` characters after `p0` as a string.
   (define (get p0 span0)
     (let (p span) (clip-range p0 span0))
     (string<-list (each get-char-after (range<- p (+ p span)))))

   ;; Replace the `span` characters after `p` by `replacement`.
   (define (replace p0 span0 replacement)
     (let (p span) (clip-range p0 span0))
     XXX)

   (export
     clip
     get
;;     size                               ;XXX bind to (given () size.^)
     replace))

(export
  backward
  forward
  nowhere
  text<-)
