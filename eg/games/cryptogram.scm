;; A UI for cryptogram puzzles.
;; Ported from github.com/darius/sturm.

(import (use 'sturm)
  cbreak-mode get-key render
  cursor green red unstyled)
(import (use 'random)
  random-rng<-)

(to (main args)
  (let cryptogram
    (may args.rest
      (be '()     (random-encrypt (random-rng<-) (run-fortune)))
      (be `(,str) str)
      (else       (error ("Usage: ~d [cryptogram]" .format (args 0))))))
  (for cbreak-mode ()
    (puzzle cryptogram)))

(let alphabet (#\a .to #\z))

(to (random-encrypt rng text)
  (let values (hey (array<-list alphabet)
                   (-> (rng .shuffle! it))))
  (let code (map<- (zip alphabet values)))
  (string<-list (for each ((ch text.lowercase))
                  (code .get ch ch))))

(to (run-fortune)
  ;; TODO ensure fits in sturm's width
  (shell-run "exec fortune"))

(to (shell-run command)                 ;; TODO extract to a library
  (let `(,from-stdout ,to-stdin ,pid) (open-subprocess command))
  ;; TODO do we have to wait for it to terminate?
  ;; XXX catch subprocess errors
  from-stdout.read-all)

(to (puzzle cryptogram)
  (let cv (cryptoview<- cryptogram))
  (begin playing ()
    (render (cv .view #yes))
    (may (get-key)
      (be 'esc (render (cv .view #no)))
      (be key
        (may key
          (be 'home      cv.go-to-start)
          (be 'end       cv.go-to-end)
          (be 'left      (cv .shift-by -1))
          (be 'right     (cv .shift-by  1))
          (be 'up        (cv .shift-line -1))
          (be 'down      (cv .shift-line  1))
          (be 'shift-tab (cv .shift-to-space -1))
          (be #\tab      (cv .shift-to-space  1))
          (be 'backspace (cv .shift-by -1)
                         (cv .jot #\space))
          (be 'del       (cv .jot #\space)
                         (cv .shift-by 1))
          (be (and (? char?) (? _.uppercase?))
                         (cv .shift-to-code 1 key))
          (else          (when (or (= key #\space) (alphabet .find? key))
                           (cv .jot key)
                           (cv .shift-by 1))))
        (playing)))))

(to (cryptoview<- cryptogram)

  (let code (those _.letter? cryptogram.uppercase))
  (surely (not code.empty?))            ;XXX 'require' or something
  (let decoder (for map-by ((_ code.range.keys))
                 #\space))
  (let point (box<- 0))                ; Index in `code` of the cursor

  (let lines (each clean cryptogram.uppercase.split-lines))
  (let line-starts
    (_.range (running-sum (for each ((line lines))
                            (tally _.letter? line)))))

  (to (shift-by offset)
    (point .^= ((+ point.^ offset) .modulo code.count)))

  (to (shift-till offset stop?)
    (shift-by offset)
    (unless (stop?)
      (shift-till offset stop?)))

  (make _

    (to (_ .jot letter)
      (decoder .set! (code point.^) letter))

    (to _.go-to-start
      (point .^= 0))

    (to _.go-to-end
      (point .^= (- code.count 1)))

    (to (_ .shift-by offset)
      (shift-by offset))

    (to (_ .shift-line offset)
      (shift-till offset (: (line-starts .maps? point.^))))

    (to (_ .shift-to-space offset)
      (when (decoder .find? #\space)
        (shift-till offset (: (= #\space (decoder (code point.^)))))))

    (to (_ .shift-to-code offset letter)
      (when (code .find? letter)
        (shift-till offset (: (= letter (code point.^))))))

    (to (_ .view show-cursor?)
      (let counts (hey (bag<- decoder.values)
                       (-> (it .delete! #\space))))
      (let clashes (_.range (for where ((n counts))
                              (< 1 n))))
      (let letters-left (for each ((ch alphabet))
                          (if (counts .maps? ch) #\space ch)))

      (let pos (box<- 0))

      (let view (flexarray<-))
      (to (emit x) (view .push! x))

      (emit (green ["Free: " letters-left "\n"]))
      (for each! ((line lines))
        (emit #\newline)
        (for each! ((ch line))
          (when (and show-cursor? ch.letter?)
            (when (= pos.^ point.^)
              (emit cursor))
            (pos .update _.+))
          (emit (decoder .get ch ch)))
        (emit #\newline)
        (for each! ((ch line))
          (emit (if ch.letter? #\- #\space)))
        (emit #\newline)
        (for each! ((ch line))
          (let color (hm (if (clashes .maps? (decoder .get ch)) red)
                         (if (= ch (code point.^))              green)
                         (else                                  unstyled)))
          (emit (color ch)))
        (emit #\newline))

      view.values)))

;; Expand tabs; blank out other control characters.
(to (clean str)
  (let r (flexarray<-))
  (for each! ((ch str))
    (hm (when (= ch #\tab)
          (begin padding ()
            (r .push! #\space)
            (unless (8 .divides? r.count)
              (padding))))
        (when (< ch #\space)
          (r .push! #\space))
        (else
          (r .push! ch))))
  (string<-list r.values))

(to (running-sum numbers)
  (let sums (flexarray<- 0))
  (for each! ((n numbers))              ;TODO scanl
    (sums .push! (+ sums.last n)))
  sums)

(export main)
