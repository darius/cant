;;; ANSI terminal control


;; I/O modes -- Unix specific, not ANSI

(to (raw-mode fn)    (mode "raw" fn))
(to (cbreak-mode fn) (mode "cbreak" fn))

(to (mode name fn)
  (system/must-succeed ("stty ~d -echo" .format name))
  (unwind-protect
   (:
     (display home)
     (display clear-to-bottom)
     (fn)
     (display cursor-show)
     (display #\newline))
   (:
     (system/must-succeed "stty sane"))))    ;TODO save & restore instead


;; ANSI terminal escape codes

(let esc (string<- (char<- 27))) ;TODO support more escapes in string literals

(let prefix (chain esc "["))

(to (seq string) (chain prefix string))

(let home               (seq "H"))
(let clear-to-bottom    (seq "J"))
(let clear-screen       (chain prefix "2J" home))
(let clear-to-right     (seq "K"))

(let cursor-pos-save    (seq "s"))
(let cursor-pos-restore (seq "u"))
(let cursor-show        (seq "?25h"))
(let cursor-hide        (seq "?25l"))

(to (sgr num)
  (seq ("~wm" .format num)))

(let [screen-height screen-width] [25 80]) ;TODO find out for real


;; Rendering

(to (render scene)
  (let cursor-seen? (box<- #no))
  (display home-and-hide)
  (paint cursor-seen? default-state scene)
  (display clear-to-bottom)
  (when cursor-seen?.^
    (display restore-and-show))
  ;; TODO do we need to flush the output?
  ;; TODO need to reset styles so they go away on program exit?
  )

(let home-and-hide    (chain home cursor-pos-save cursor-hide))
(let restore-and-show (chain cursor-pos-restore cursor-show))
(let cr-lf            (chain clear-to-right "\r\n"))

(make state<-
  (to (and (_ fg bg styles) tuple)
    (make state
      (to _.reveal            tuple)
      (to (_ .set-fg code)    (state<- code bg   styles))
      (to (_ .set-bg code)    (state<- fg   code styles))
      (to (_ .add-style code) (state<- fg   bg   (styles .or code))))))

(let default-state (state<- 39 49 0))

(to (screen-state<-)
  (let fg     (box<- 39))
  (let bg     (box<- 49))
  (let styles (box<- 0))
  (to (_ .establish! state)
    (let (_ want-fg want-bg want-styles) state.reveal)
    (unless (= styles.^ want-styles)
      (display (sgr 0))
      (fg .^= 39)
      (bg .^= 49)
      (for each! ((s '(1 4 5 7)))
        (unless (= 0 (want-styles .and (1 .<< s)))
          (display (sgr s))))
      (styles .^= want-styles))
    (unless (= want-fg fg.^)
      (display (sgr want-fg))
      (fg .^= want-fg))
    (unless (= want-bg bg.^)
      (display (sgr want-bg))
      (bg .^= want-bg))))

(let screen-state (screen-state<-))

(to (paint cursor-seen? wanted-state scene)
  ;; TODO: skip any terminal codes in the scene's strings/chars
  (may scene
    (be (? string?)
      (screen-state .establish! wanted-state)
      (for each! ((ch scene))
        (display (if (= ch #\newline) cr-lf ch))))
    (be (? char?)
      (screen-state .establish! wanted-state)
      (display (if (= scene #\newline) cr-lf scene)))
    (be (? (-> (or (list? it) (array? it))))
      (for each! ((subscene scene.values))
        (paint cursor-seen? wanted-state subscene)))
    (else
      (scene .paint cursor-seen? wanted-state))))

(to (cursor .paint cursor-seen? _)
  (display cursor-pos-save)
  (cursor-seen? .^= #yes))

(to (foreground-color<- code)
  (to (foreground-color subscene)
    (to (fg-painter .paint cursor-seen? wanted-state)
      (paint cursor-seen? (wanted-state .set-fg code) subscene))))

(to (background-color<- code)
  (to (background-color subscene)
    (to (bg-painter .paint cursor-seen? wanted-state)
      (paint cursor-seen? (wanted-state .set-bg code) subscene))))

(to (style<- code)
  (let mask (1 .<< code))
  (to (style subscene)
    (to (style-painter .paint cursor-seen? wanted-state)
      (paint cursor-seen? (wanted-state .add-style mask) subscene))))

(let `(,black ,red ,green ,yellow ,blue ,magenta ,cyan ,white)
  (each foreground-color<- (30 .to 37)))

(let `(,on-black ,on-red ,on-green ,on-yellow ,on-blue ,on-magenta ,on-cyan ,on-white)
  (each background-color<- (40 .to 47)))

(let bold       (style<- 1))
(let underlined (style<- 4))
(let blinking   (style<- 5))
(let inverted   (style<- 7))

(to (unstyled scene) scene)
    

;; Keyboard input
;; TODO optional timeout

(to (ctrl ch)
  (- ch.uppercase 64))

(let key-map
  (map<- `((,(string<- (ctrl #\X))   esc) ;XXX a hack until the timeout works
           (,(string<- (char<- 127)) backspace)

           (,(chain esc "[1~")  home)   (,(chain esc "[H") home)
           (,(chain esc "[2~")  ins)
           (,(chain esc "[3~")  del)
           (,(chain esc "[4~")  end)    (,(chain esc "[F") end)
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

           (,(chain esc "[A") up)       (,(chain esc "[OA") up)
           (,(chain esc "[B") down)     (,(chain esc "[OB") down)
           (,(chain esc "[C") right)    (,(chain esc "[OC") right)
           (,(chain esc "[D") left)     (,(chain esc "[OD") left)
           )))

(let key-map-prefixes
  (_.range (for gather ((full-key key-map.keys))
             (for each ((i (1 .to< full-key.count)))
               (full-key .slice 0 i)))))

(let key-stack (flexarray<-))

(to (get-key-unmapped)
  (if key-stack.some?
      key-stack.pop!
      (hey stdin.read-char
           (-> (surely (not (eof? it))))))) ;shouldn't ever happen in raw/cbreak modes

(make get-key
  (to (_)
    (let keys (flexarray<- (get-key-unmapped)))
    (begin matching ()
      (let s (string<-list keys))
      (hm (or (key-map .get s))
          (when (key-map-prefixes .maps? s)
            (keys .push! (get-key-unmapped))
            (matching))
          (else
            (key-stack .extend! (reverse keys)) ;TODO use a FIFO instead of this reversed LIFO
            key-stack.pop!))))
  ;; Silly conveniences:
  (to _.uppercase ((get-key) .uppercase))
  (to _.lowercase ((get-key) .lowercase)))

(export
  raw-mode cbreak-mode
  screen-height screen-width
  get-key ctrl
  render
  cursor
  bold underlined blinking inverted unstyled
  black red green yellow blue magenta cyan white
  on-black on-red on-green on-yellow on-blue on-magenta on-cyan on-white)
