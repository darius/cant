;; ANSI terminal control
;; XXX untested

(let prefix (string<- (char<- 27) #\[)) ;TODO more string escapes

(define (seq string) (chain prefix string))

(let home               (seq "H"))
(let clear-to-bottom    (seq "J"))
(let clear-screen       (seq "2J" home))
(let clear-to-eol       (seq "K"))

(let save-cursor-pos    (seq "s"))
(let restore-cursor-pos (seq "u"))

(let show-cursor        (seq (string<- (char<- 25) #\h)))
(let hide-cursor        (seq (string<- (char<- 25) #\l)))

(define (goto x y)
  (chain prefix
         (string<-number (+ y 1)) ";"
         (string<-number (+ x 1)) "H"))

(let (black red green yellow blue magenta cyan white)
  (chain (range<- 8) '()))              ;TODO ugly

(define (bright color)
  (+ 60 color))

(define (sgr num)
  (seq (string<-number num) "m")) ;TODO format to string

(define (set-foreground color) (sgr (+ 30 color)))
(define (set-background color) (sgr (+ 40 color)))

(export
  home
  clear-to-bottom
  clear-screen
  clear-to-eol
  save-cursor-pos
  restore-cursor-pos
  show-cursor
  hide-cursor
  goto 
  black red green yellow blue magenta cyan white
  bright
  set-foreground
  set-background)
