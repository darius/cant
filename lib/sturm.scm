;;; ANSI terminal control


;; I/O modes -- Unix specific, not ANSI

(define (raw-mode fn)    (mode "raw" fn))
(define (cbreak-mode fn) (mode "cbreak" fn))

(define (mode name fn)
  ;; TODO note screen size
  (match (system ("stty %d -echo" .format name))
    (0 'ok))
  (display home)
  (display clear-to-bottom)
  (fn)                                  ;TODO unwind-protect
  (display cursor-show)
  (display #\newline)
  (match (system "stty sane")           ;XXX save & restore instead
    (0 'ok)))


;; ANSI terminal escape codes

(let prefix (string<- (char<- 27) #\[ )) ;TODO more string escapes

(define (seq string) (chain prefix string))

(let home               (seq "H"))
(let clear-to-bottom    (seq "J"))
(let clear-screen       (chain prefix "2J" home))
(let clear-to-right     (seq "K"))

(let cursor-pos-save    (seq "s"))
(let cursor-pos-restore (seq "u"))
(let cursor-show        (seq "?25h"))
(let cursor-hide        (seq "?25l"))

;(let (black red green yellow blue magenta cyan white)
;  (chain (range<- 8) '()))              ;TODO ugly

(define (bright color)
  (+ 60 color))

(define (sgr num)
  (seq (string<-number num) "m")) ;TODO format to string

(define (set-foreground color) (sgr (+ 30 color)))
(define (set-background color) (sgr (+ 40 color)))


;; Rendering

(let home-and-hide    (chain home cursor-pos-save cursor-hide))
(let restore-and-show (chain cursor-pos-restore cursor-show))
(let crlf             (chain clear-to-right "\r\n"))

(define (render view)
  (display home-and-hide)
  (begin rendering ((v view))
    ;; TODO actual terminal codes
    ;; TODO replace newlines
    (case ((string? v)
           (for each! ((ch v))
             (display (if (= ch #\newline) crlf ch))))
          ((char? v)
           (display (if (= v #\newline) crlf v)))
          ((list? v)
           (each! rendering v))
          (else
           (error "Can't render" v))))
  (display clear-to-bottom)
  ;; TODO check if cursor seen
  (display restore-and-show)
  ;; TODO do we need to flush the output?
  )

;; TODO actual terminal codes
(define (green str) str)

(define (color c) c)
(define ((compose style-1 style-2) c)
  c)
(define (bold c) c)
(define (unstyled c) c)


;; Keyboard input

(define (get-key)
  (__read-char))                        ;TODO parse escape codes, etc.


(export
  raw-mode cbreak-mode
  get-key render
  color green compose bold unstyled)
