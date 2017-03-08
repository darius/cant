;; A UI for cryptogram puzzles.
;; Ported from github.com/darius/sturm.

(import (use "lib/sturm")
  cbreak-mode get-key render
  cursor green red unstyled)
(import (use "lib/bag")
  bag<-)

(define (main args)
  (let cryptogram
    (match args.rest
      (()    (random-encrypt (run-fortune)))
      ((str) str)
      (_     (error ("Usage: %d [cryptogram]" .format (args 0))))))
  (for cbreak-mode ()
    (puzzle cryptogram)))

(let alphabet (each char<- (range<- 97 122))) ;XXX clumsy

(define (random-encrypt text)
  (let values (vector<-list alphabet))
  (random-shuffle! values)               ;XXX
  (let code (call map<- (zip alphabet values))) ;XXX
  (string<-list (for each ((ch text.lower))
                  (code .get ch ch))))

(define (run-fortune)
  ;; XXX ensure fits in num-cols
  ((shell-run "fortune") .split-lines))

(define (shell-run command)
  (system command)
  ;; XXX get the output
  )

(define (puzzle cryptogram)
  (let cv (cryptoview<- cryptogram))
  (begin playing ()
    (render (cv .view #yes))
    (let key (get-key))
    (match key
      ('esc       (render (cv .view #no)))
      ('home      cv.go-to-start                            (playing))
      ('end       cv.go-to-end                              (playing))
      ('left      (cv .shift-by -1)                         (playing))
      ('right     (cv .shift-by  1)                         (playing))
      ('up        (cv .shift-line -1)                       (playing))
      ('down      (cv .shift-line  1)                       (playing))
      (#\tab      cv.shift-to-space                         (playing))
      ('backspace (cv .shift-by -1)    (cv .jot #\space)    (playing))
      ('del       (cv .jot #\space)    (cv .shift-by 1)     (playing))
      (_
       (when (or key.whitespace? (alphabet .find? key))
         (cv .jot key)
         (cv .shift-by 1))
       (playing)))))

(define (cryptoview<- cryptogram)

  (let code (filter '.letter? cryptogram))
  (surely (not code.empty?))
  (let decoder
    (map<-a-list (for each ((ch ((call set<- code) .keys))) ;XXX clumsy
                   `(,ch #\space))))
  (let lines (each clean cryptogram.split-lines))
  (let cursor-at (box<- 0))

  (define (shift-by offset)
    (cursor-at .^= ((+ cursor-at.^ offset) .modulo code.count)))

  (define (line-number pos)
    (let items
      (for filter (((i start) line-starts.items))
        (and (<= start pos) (< pos (line-starts (+ i 1))))))
    (match items.first                  ;TODO should be lazy
      ((i _) i)))

  (let line-starts
    (running-sum (for each ((line lines))
                   ((filter '.letter? line) .count))))

  (make _
    ({.jot letter}
     (decoder .set! (code cursor-at.^) letter))

    ({.go-to-start}
     (cursor-at .^= 0))

    ({.go-to-end}
     (cursor-at .^= (- code.count 1)))

    ({.shift-by n}
     (shift-by n))

    ({.shift-line offset}
     (let line-num ((+ (line-number cursor-at.^) offset)
                    .modulo lines.count))
     (cursor .^= (line-starts line-num)))

    ({.shift-to-space}
     (when (decoder.values .find? #\space) ;XXX probably won't work yet
       (begin shifting ()
         (shift-by 1)
         (unless (= #\space (decoder (code cursor-at.^)))
           (shifting)))))

    ({.view show-cursor?}
     (let counts (call bag<- (for filter ((v decoder.values))
                               (not= v #\space))))
     (let letters-left (for each ((ch alphabet))
                         (if (counts .maps? ch) #\space ch)))
     (let clashes (call set<- (for gather (((v n) counts.items))
                                (if (< 1 n) `(,v) '()))))

     (let pos (box<- 0))

     (let view (fillvector<-))
     (define (emit x) (view .push! x))

     (emit (green `("Free: " ,letters-left #\newline)))
     (for each! ((line lines))
       (emit #\newline)
       (for each! ((ch line))
         (when (and show-cursor? ch.letter?)
           (when (= pos.^ cursor-at.^)
             (emit cursor))
           (pos .^= (+ pos.^ 1)))         ;XXX clumsier
         (emit (decoder .get ch ch)))
       (emit #\newline)
       (for each! ((ch line))
         (emit (if ch.letter? #\- #\space)))
       (emit #\newline)
       (for each! ((ch line))
         (let color (case ((clashes .maps? (decoder .get ch)) red)
                          ((= ch (code cursor-at.^))          green)
                          (else                               unstyled)))
         (emit (color ch)))
       (emit #\newline))

     (as-list view))))

;; Expand tabs; blank out other control characters.
(define (clean str)
  (let r (fillvector<-))
  (for each! ((ch str))
    (case ((= ch #\tab)
           (begin padding ()
             (r .push! #\space)
             (unless (= 0 (r.count .modulo 8))
               (padding))))
          ((< ch.code 32)
           (r .push! #\space))
          (else
           (r .push! ch))))
  (string<-list (as-list r)))           ;XXX clumsy

(define (running-sum numbers)
  (let sums (fillvector<- 0))
  (for each! ((n numbers))
    (sums .push! (+ sums.last n)))
  sums)

(export main)
