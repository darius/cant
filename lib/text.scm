;; Ported from github.com/darius/dole

;; Glossary:
;;   p   coordinate (position between characters in text, or `nowhere`)
;;   dir direction
;;   cs  charset

;; A text is a sequence of characters. Coordinates denote the spaces
;; *between* character positions (or before or after them, at the
;; ends), in [0..size]. The coordinate before the first character is
;; 0, then after the first character and before the second is 1, and
;; so on, until the coordinate after the last character is `size`.
;; 
;; We store the characters at integer indices in a fillvector
;; (starting at index 0) in two chunks which we call the head and the
;; tail, separated by the gap. The fields `head` and `gap` (and `tail`
;; if it weren't implicit) denote the lengths of these spans. `size`
;; denotes head+tail, i.e. the total length of text. The whole array
;; is of size size+gap, i.e. head+gap+tail.
;; 
;; The gap lets us insert or delete text by moving the gap instead
;; of the whole tail; if there's locality, this will be cheaper.


;; A coordinate that's never an actual text position. We'll use
;; -nowhere for "off the left end" and +nowhere for the right.
(let nowhere (expt 2 40))               ;XXX a dangerous pretense

;; Directions from a coordinate.
(let backward -1)
(let forward   1)

(to (text<-)

  (let t    (fillvector<-))
  (let head (box<- 0))
  (let gap  (box<- 0))
  (let size (box<- 0))

  ;; Return coordinate p clipped to the text's actual range.
  (to (clip p)
    (max 0 (min p size.^)))

  (to (clip-range p span)
    (let q (clip p))
    (let h (clip (+ q (max 0 span))))
    `(,q ,(- h q)))

  ;; Pre: p is in [0..size).
  (to (get-char-after p)
    (t (if (< p head.^) p (+ p gap.^))))

  ;; Return the `span` characters after `p0` as a string.
  (to (get p0 span0)
    (let (p span) (clip-range p0 span0))
    (string<-list (each get-char-after (range<- p (+ p span)))))

  (to (grow n)
    (+ n (n .quotient 2)))

  ;; Replace the `span` characters after `p` by `replacement`.
  ;; TODO this code is hard to follow
  ;; XXX check/correct this for 0-based vectors
  (to (replace p0 span0 replacement)
    (let (p span) (clip-range p0 span0))
     
    ;; Make position p start the tail.
    (if (<= p head.^)
        (t .move! (+ gap.^ p)  p                 (- head.^ p))
        (t .move! head.^       (+ gap.^ head.^)  (- p head.^)))
    (head .^= p)

    ;; Delete the next `span` characters.
    (gap .^= (+ gap.^ span))
    (size .^= (- size.^ span))

    ;; Grow the array so `replacement` fits in the gap.
    (let r-size replacement.count)
    (when (< gap.^ r-size)
      (let tail (- size.^ head.^)) ;XXX shouldn't this subtract the gap too?
      (let sz   (+ (grow (+ size.^ gap.^))   ;; TODO rename
                   r-size))
      (t .resize! sz)
      (t .move! (- sz tail) (+ head.^ gap.^) tail)
      (gap .^= (- sz size.^)))

    ;; Insert `replacement`.
    ;; (t .copy! head.^ replacement 0 r-size)   TODO no such method
    (for each! ((i (range<- r-size)))
      (t .set! (+ i head.^) (replacement i)))
    (head .^= (+ head.^ r-size))
    (gap  .^= (- gap.^ r-size))
    (size .^= (+ size.^ r-size)))

  (make _
    ({.clear}
     (head .^= 0)
     (gap  .^= 0)
     (size .^= 0))
    ({.clip p}             (clip p))
    ({.get p span}         (get p span))
    ({.delete p span}      (replace p span ""))
    ({.insert p str}       (replace p 0 str))
    ({.replace p span str} (replace p span str))
    ({.count}              size.^)
    ;; TODO: delegate to a string trait? but strings are immutable...
    ))

(to (main _)
  (let t (text<-))
  (print (t .get 0 10))
  (t .replace 0 0 "hello")
  (print (t .get 0 10))
  (t .replace 1 2 "GOOBER")
  (print (t .get 0 20))
  (print (t .get 3 3))
  )

(export
  backward
  forward
  nowhere
  text<-)
