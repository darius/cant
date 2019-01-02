;; A UI for cryptogram puzzles.
;; Ported from github.com/darius/sturm.

(import (use "lib/sturm")
  cbreak-mode get-key render
  cursor green red unstyled)
(import (use "lib/random")
  random-rng<-)

(to (main args)
  (let cryptogram
    (match args.rest
      ('()     (random-encrypt (random-rng<-) (run-fortune)))
      (`(,str) str)
      (_       (error ("Usage: ~d [cryptogram]" .format (args 0))))))
  (for cbreak-mode ()
    (puzzle cryptogram)))

(let alphabet (as-list (#\a .to #\z)))

(to (random-encrypt rng text)
  (let values (array<-list alphabet))
  (rng .shuffle! values)
  (let code (map<- (zip alphabet values.values))) ;XXX why .values needed?
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
    (match (get-key)
      ('esc (render (cv .view #no)))
      (key (match key
             ('home      cv.go-to-start)
             ('end       cv.go-to-end)
             ('left      (cv .shift-by -1))
             ('right     (cv .shift-by  1))
             ('up        (cv .shift-line -1))
             ('down      (cv .shift-line  1))
             ('shift-tab (cv .shift-to-space -1))
             (#\tab      (cv .shift-to-space  1))
             ('backspace (cv .shift-by -1)
                         (cv .jot #\space))
             ('del       (cv .jot #\space)
                         (cv .shift-by 1))
             ((and (? char?)
                   (? '.uppercase?))
                         (cv .shift-to-code 1 key))
             (_          (when (or (= key #\space) (alphabet .find? key))
                           (cv .jot key)
                           (cv .shift-by 1))))
           (playing)))))

(to (cryptoview<- cryptogram)

  (let code (those '.letter? cryptogram.uppercase))
  (surely (not code.empty?))            ;XXX 'require' or something
  (let decoder
    (map<- (for each ((ch code.range.keys))
             `(,ch #\space))))
  (let point (box<- 0))                ; Index in `code` of the cursor

  (let lines (each clean cryptogram.uppercase.split-lines))
  (let line-starts
    ('.range (running-sum (for each ((line lines))
                            (tally '.letter? line)))))

  (to (shift-by offset)
    (point .^= ((+ point.^ offset) .modulo code.count)))

  (to (shift-till offset stop?)
    (shift-by offset)
    (unless (stop?)
      (shift-till offset stop?)))

  (make _
    ({.jot letter}
     (decoder .set! (code point.^) letter))

    ({.go-to-start}
     (point .^= 0))

    ({.go-to-end}
     (point .^= (- code.count 1)))

    ({.shift-by offset}
     (shift-by offset))

    ({.shift-line offset}
     (shift-till offset (given () (line-starts point.^))))

    ({.shift-to-space offset}
     (when (decoder .find? #\space)
       (shift-till offset (given () (= #\space (decoder (code point.^)))))))

    ({.shift-to-code offset letter}
     (when (code .find? letter)
       (shift-till offset (given () (= letter (code point.^))))))

    ({.view show-cursor?}
     (let counts (call bag<- decoder.values))
     (let letters-left (for each ((ch alphabet))
                         (if (counts .maps? ch) #\space ch)))
     (let clashes ('.range (for filter ((`(,v ,n) counts.items))
                             (and (< 1 n) (not= v #\space) v))))

     (let pos (box<- 0))

     (let view (flexarray<-))
     (to (emit x) (view .push! x))

     (emit (green `("Free: " ,letters-left #\newline)))
     (for each! ((line lines))
       (emit #\newline)
       (for each! ((ch line))
         (when (and show-cursor? ch.letter?)
           (when (= pos.^ point.^)
             (emit cursor))
           (pos .^= (+ pos.^ 1)))         ;XXX clumsier
         (emit (decoder .get ch ch)))
       (emit #\newline)
       (for each! ((ch line))
         (emit (if ch.letter? #\- #\space)))
       (emit #\newline)
       (for each! ((ch line))
         (let color (case ((clashes .maps? (decoder .get ch)) red)
                          ((= ch (code point.^))              green)
                          (else                               unstyled)))
         (emit (color ch)))
       (emit #\newline))

     view.values)))

;; Expand tabs; blank out other control characters.
(to (clean str)
  (let r (flexarray<-))
  (for each! ((ch str))
    (case ((= ch #\tab)
           (begin padding ()
             (r .push! #\space)
             (unless (8 .divides? r.count)
               (padding))))
          ((< ch #\space)
           (r .push! #\space))
          (else
           (r .push! ch))))
  (string<-list r.values))           ;XXX clumsy

(to (running-sum numbers)
  (let sums (flexarray<- 0))
  (for each! ((n numbers))              ;TODO scanl
    (sums .push! (+ sums.last n)))
  sums)

(export main)
