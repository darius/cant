;; ANSI terminal control
;; TODO factor wrt sturm

(let prefix (string<- (char<- 27) #\[)) ;TODO more string escapes

(to (seq string) (chain prefix string))

(let home               (seq "H"))
(let clear-to-bottom    (seq "J"))
(let clear-screen       (chain prefix "2J" home))  ;XXX don't actually need `home`, right?
(let clear-to-right     (seq "K"))

(let cursor-pos-save    (seq "s"))
(let cursor-pos-restore (seq "u"))

(let cursor-show        (seq "?25h"))
(let cursor-hide        (seq "?25l"))

(to (goto x y)
  (chain prefix
         (string<-number (+ y 1)) ";"
         (string<-number (+ x 1)) "H"))

(let `(,black ,red ,green ,yellow ,blue ,magenta ,cyan ,white)
  (as-list (0 .to 7)))              ;TODO ugly

(to (bright color)
  (+ 60 color))

(to (sgr num)
  (seq (string<-number num) "m")) ;TODO format to string

(to (set-foreground color) (sgr (+ 30 color)))
(to (set-background color) (sgr (+ 40 color)))

(export
  home
  clear-to-bottom
  clear-screen
  clear-to-right
  cursor-pos-save
  cursor-pos-restore
  cursor-show
  cursor-hide
  goto 
  black red green yellow blue magenta cyan white
  bright
  set-foreground
  set-background)
