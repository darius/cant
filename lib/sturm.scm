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

(let esc (string<- (char<- 27))) ;TODO support more escapes in string literals

(let prefix (chain esc "["))

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
  (seq ("%wm" .format num)))

(define (set-foreground color) (sgr (+ 30 color)))
(define (set-background color) (sgr (+ 40 color)))


;; Rendering

(let home-and-hide    (chain home cursor-pos-save cursor-hide))
(let restore-and-show (chain cursor-pos-restore cursor-show))
(let crlf             (chain clear-to-right "\r\n"))

(make cursor)

(define (render view)
  (let cursor-seen? (box<- #no))
  (display home-and-hide)
  (begin rendering ((v view))
    ;; TODO actual terminal codes
    (case ((string? v)
           (for each! ((ch v))
             (display (if (= ch #\newline) crlf ch))))
          ((char? v)
           (display (if (= v #\newline) crlf v)))
          ((list? v)
           (each! rendering v))
          ((= v cursor)
           (display cursor-save)
           (cursor-seen? .^= #yes))
          (else
           (error "Can't render" v))))
  (display clear-to-bottom)
  (when cursor-seen?.^
    (display restore-and-show))
  ;; TODO do we need to flush the output?
  )

;; TODO actual terminal codes
(define (green str) str)

(define (color c) c)
(define (bold c) c)
(define (unstyled c) c)


;; Keyboard input
;; TODO optional timeout

(let key-map
  (map<-a-list `((,(string<- (char<- 127)) backspace)

                 (,(chain esc "[1~")  home)
                 (,(chain esc "[2~")  ins)
                 (,(chain esc "[3~")  del)
                 (,(chain esc "[4~")  end)
                 (,(chain esc "[5~")  pg-up)
                 (,(chain esc "[6~")  pg-dn)
                 (,(chain esc "[11~") f1)
                 (,(chain esc "[12~") f2)
                 (,(chain esc "[13~") f3)
                 (,(chain esc "[14~") f4)
                 (,(chain esc "[15~") f5)
                 (,(chain esc "[17~") f6)
                 (,(chain esc "[18~") f7)
                 (,(chain esc "[19~") f8)
                 (,(chain esc "[20~") f9)
                 (,(chain esc "[21~") f10)
                 (,(chain esc "[23~") f11)
                 (,(chain esc "[24~") f12)
                 (,(chain esc "[Z")   shift-tab)

                 (,(chain esc "[A") up)     (,(chain esc "[OA") up)
                 (,(chain esc "[B") down)   (,(chain esc "[OB") down)
                 (,(chain esc "[C") right)  (,(chain esc "[OC") right)
                 (,(chain esc "[D") left)   (,(chain esc "[OD") left)
                 )))

(let key-map-prefixes
  (call set<- (for gather ((full-key key-map.keys))
                (for each ((i (range<- 1 full-key.count)))
                  (full-key .slice 0 i)))))

(let key-stack (fillvector<-))

(define (get-key-unmapped)
  (if key-stack.empty?
      (do (let ch stdin.read-char)
          (assert (not (eof-object? ch))) ;shouldn't ever happen in raw/cbreak modes
          ch)
      key-stack.pop!))

(define (get-key)
  (let keys (fillvector<- (get-key-unmapped)))
  (begin matching ()
    (let s (string<-list (as-list keys))) ;XXX clumsy
    (or (key-map .get s)
        (case ((key-map-prefixes .maps? s)
               (let next-key (get-key-unmapped))
               (keys .push! next-key)
               (matching))
              (else
               (key-stack .extend! (reverse keys))
               key-stack.pop!)))))

(export
  raw-mode cbreak-mode
  get-key render cursor
  color green bold unstyled)
