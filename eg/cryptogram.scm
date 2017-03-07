;; A UI for cryptogram puzzles.
;; Ported from github.com/darius/sturm.

(import (use "lib/sturm")
  cbreak-mode get-key render cursor)

(define (main args)
  (let make-cryptogram
    (match args.rest
      (()    (given () (random-encrypt (run-fortune))))
      ((str) (given () str))
      (_     (error ("Usage: %d [cryptogram]" .format (args 0))))))
  (for cbreak-mode ()
    (puzzle (make-cryptogram))))

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
  XXX)
