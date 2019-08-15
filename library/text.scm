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
;; We store the characters at integer indices in a flexarray
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

  (let t    (flexarray<-))
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
    (let `(,p ,span) (clip-range p0 span0))
    (string<-list (each get-char-after (p .span span))))

  ;; Return the position after the character in the text that is in
  ;; char-set and that comes up first in searching from p in
  ;; direction dir. If none, return nowhere or -nowhere.
  ;; XXX untested, probably broken
  (to (find-char-set p dir char-set)
    ((if (= dir forward)
         find-char-set-forward
         find-char-set-backward)
     (clip p)
     char-set))

  (to (find-char-set-forward p char-set)
    (let H head.^)
    (begin searching ((p p))
      (if (< p H)
          (if (char-set .maps? (t p))
              p.+
              (searching p.+))
          (do
            (let G gap.^)
            (let S size.^)
            (begin persevering ((p p))
              (if (< p S)
                  (if (char-set .maps? (t (+ G p)))
                      p.+
                      (persevering p.+))
                  nowhere))))))

  (to (find-char-set-backward p char-set)
    (let H head.^)
    (let G gap.^)
    (begin searching ((p p))
      (if (< H p)
          (if (char-set .maps? (t (+ G p.-)))
              p
              (searching p.-))
          (begin persevering ((p p))
            (if (< 0 p)
                (if (char-set .maps? (t p.-))
                    p
                    (persevering p.-))
                (- nowhere))))))

  (to (grow n)
    (+ n (n .quotient 2)))

  ;; Replace the `span` characters after `p` by `replacement`.
  ;; TODO this code is hard to follow
  (to (replace p0 span0 replacement)
    (let `(,p ,span) (clip-range p0 span0))
     
    ;; Make position p start the tail.
    (if (<= p head.^)
        (t .move! (+ gap.^ p)  t p                head.^)
        (t .move! head.^       t (+ gap.^ head.^) (+ gap.^ p)))
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
      (t .move! (- sz tail) t (+ head.^ gap.^) (+ size.^ gap.^)) ;TODO isn't the last just t.count ?
      (gap .^= (- sz size.^)))

    ;; Insert `replacement`.
    (t .move! head.^ replacement 0 r-size)

    (head .^= (+ head.^ r-size))
    (gap  .^= (- gap.^ r-size))
    (size .^= (+ size.^ r-size)))

  (make text
    (to _.clear!
      (head .^= 0)
      (gap  .^= 0)
      (size .^= 0))
    (to (_ .clip p)             (clip p))
    (to (_ .get p span)         (get p span))
    (to (_ .delete p span)      (replace p span "")) ;TODO rename these with !
    (to (_ .insert p str)       (replace p 0 str))
    (to (_ .replace p span str) (replace p span str))
    (to _.count                 size.^)
    (to (_ .find-char-set p dir char-set)
      (find-char-set p dir char-set))
    ;; TODO: delegate to a string trait? but strings are immutable...
    (to _.keys
      (0 .to< size.^))
    ))

(export
  backward
  forward
  nowhere
  text<-)
