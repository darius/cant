;; A UI for cryptogram puzzles.
;; Ported from github.com/darius/sturm.

(import (use "lib/sturm")
  cbreak-mode get-key render cursor)

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

  (let code (for filter ((ch cryptogram)) ch.letter?))
  (assert (not code.empty?))
  (let decoder
    (map<-a-list (for each ((ch ((call set<- code) .keys))) ;XXX clumsy
                   `(,ch #\space))))
  (let lines (each clean cryptogram.split-lines))
  (let cursor-at (box<- 0))

  (define (jot letter)
    (decoder .set! (code cursor-at.^) letter))

  (define (shift-by offset)
    (cursor-at .^= ((+ cursor-at.^ offset) .modulo code.count)))

  (define (shift-to-space)
    (when (decoder.values .find? #\space) ;XXX probably won't work yet
      (begin shifting ()
        (shift-by 1)
        (unless (= #\space (decoder (code cursor-at.^)))
          (shifting)))))

  (define (shift-line offset)
    (let next-line ((+ (line-number cursor-at.^) offset)
                    .modulo lines.count))
    (cursor .^= (line-starts next-line)))

  (define (line-number pos)
    (let items
      (for filter (((i start) line-starts.items))
        (and (<= start pos) (< pos (line-starts (+ i 1))))))
    (match items.first                  ;TODO should be lazy
      ((i _) i)))

  (let line-starts
    (running-sum (for each ((line lines))
                   ((filter '.letter? line) .count))))

  (define (view show-cursor?)
    XXX)

  (begin playing ()
    (render (view #yes))
    (let key (get-key))
    (match key
      ('esc       (render (view #no)))
      ('home      (cursor-at .^= 0)                 (playing))
      ('end       (cursor-at .^= (- code.count 1))  (playing))
      ('left      (shift-by -1)                     (playing))
      ('right     (shift-by  1)                     (playing))
      ('up        (shift-line -1)                   (playing))
      ('down      (shift-line  1)                   (playing))
      (#\tab      (shift-to-space)                  (playing))
      ('backspace (shift-by -1)    (jot #\space)    (playing))
      ('del       (jot #\space)    (shift-by 1)     (playing))
      (_
       (when (printables .maps? key)
         (jot key)
         (shift-by 1))
       (playing)))))

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
  (string<-list r))

(define (running-sum numbers)
  (let sums (fillvector<- 0))
  (for each! ((n numbers))
    (sums .push! (+ sums.last n)))
  sums)
