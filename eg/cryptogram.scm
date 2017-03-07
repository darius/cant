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

  XXX)
