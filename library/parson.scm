;; Parson's concrete language-independent syntax
;; XXX leaving out regexes, fnord, anonymous start

(import (use 'parson-core)
  invert capture either then feed-list feed push seclude delay
  maybe many at-least-1
  fail empty end skip-1 take-1 any-1 skip-any-1 lit-1 lit drop
  parse)

(let hug (feed-list identity))

(to (rule-ref<- name)
  (list<- (set<- name)
          (on (_ rules _)
            (delay (: (rules name))))))

(to (constant<- p)
  (list<- (set<-)
          (on (_ _ _) p)))

(to (lift peg-op)
  (feed-list
   (on (lifted)
     (list<- (union-over (for each ((`(,refs ,_) lifted))
                           refs))
             (on (builder rules subs)
               (peg-op @(for each ((`(,_ ,f) lifted))
                          (f builder rules subs))))))))

(to (literal<- string)
  (list<- (set<-)
          (on (builder _ _) (builder .literal string))))

(to (keyword<- string)
  (list<- (set<-)
          (on (builder _ _) (builder .keyword string))))

(to (unquote<- name)
  (list<- (set<-)
          (on (_ _ subs) (subs name))))

(to (push-lit<- string)
  (constant<- (push string)))

(to (name-char? ch) (or ch.letter?       (= ch #\_)))
(to (word-char? ch) (or ch.alphanumeric? (= ch #\_)))

(let word-boundary (invert (skip-1 word-char?)))

(let eat-line
  (delay (: (either (lit-1 #\newline)
                    (then skip-any-1 eat-line)
                    empty))))

(let whitespace
  (at-least-1 (either (skip-1 _.whitespace?)
                      (then (lit-1 #\#) eat-line))))

(let __ (maybe whitespace))

(let name 
  (seclude
   (then (capture (then (skip-1 name-char?)
                        (many (skip-1 word-char?))))
         (feed symbol<-)
         __)))

(let word
  (seclude
   (then (capture (many (skip-1 word-char?)))
         (feed symbol<-)
         __)))

(to (string-quoted-by q-char)
  (let q (lit-1 q-char))
  (let quoted-char
    (then (either (lit-1 #\\) (invert q))
          any-1))
  (seclude
   (then q (many quoted-char) q __
         (feed chain))))           ;XXX if empty, you'll get () instead of ""

(let qstring  (string-quoted-by #\'))
(let dqstring (string-quoted-by #\"))

(let pe
  (delay (: (seclude
             (either (then term (maybe (then (lit "|") __ pe (lift either))))
                     (lift (: empty)))))))

(let term
  (delay (: (seclude
             (then factor (maybe (then term (lift then))))))))

(let factor
  (delay (: (seclude
             (either (then (lit "!") __ factor (lift invert))
                     (then primary
                           (either (then (lit "**") __ primary (lift many))
                                   (then (lit "++") __ primary (lift at-least-1))
                                   (then (lit "*") __ (lift many))
                                   (then (lit "+") __ (lift at-least-1))
                                   (then (lit "?") __ (lift maybe))
                                   empty)))))))

(let primary
  (seclude
   (either (then (lit "(") __ pe (lit ")") __)
           (then (lit "[") __ pe (lit "]") __   (lift seclude))
           (then (lit "{") __ pe (lit "}") __   (lift capture))
           (then qstring (feed literal<-))
           (then dqstring (feed keyword<-))
           (then (lit ":") (either (then word    (feed unquote<-))
                                   (then qstring (feed push-lit<-))))
           (then name (feed rule-ref<-)))))

(let rule
  (seclude
   (then name
         (either (then (lit "=") __ pe)
                 (then (lit ":") whitespace
                       (seclude (then pe (lift seclude)))))
         (lit ".") __
         hug)))

(let parson-grammar
  (then __ (at-least-1 rule) end))

(make none)

(to (union-map<- map backup)
  ;; TODO: rest of map interface?
  (on (key)
    (let value (map .get key none))
    (if (= value none)
        (backup key)
        value)))

(to (grammar<- text)
  (let skeletons (parse-grammar text))
  (let builder default-builder)         ;TODO parameterize
  (on (subs)
    (let full-subs (union-map<- subs default-subs))
    (let rules (for map-by ((name (each _.first skeletons)))
                 (delay (: (rules name)))))
    (for each! ((`(,name (,_ ,f)) skeletons)) ;XXX better name than f
      (let peg (f builder rules full-subs))
      (rules .set! name peg))
    rules))

(let default-subs
  (map<- `((skip ,skip-any-1)
           (anyone ,any-1)            ;XXX all these should probably skip instead of capture
           (letter ,(take-1 _.letter?))
           (digit ,(take-1 _.digit?))
           (end ,end)
           (hug ,hug)
           (join ,(feed chain))
           (drop ,drop)
           (whitespace ,(skip-1 _.whitespace?))
           (nat ,(seclude                                        ;TODO rename
                  (then (capture (at-least-1 (skip-1 _.digit?))) ;; TODO no leading 0s
                        (feed number<-string))))
           (int ,(seclude
                  (then (capture (then (maybe (lit-1 #\-))
                                       (at-least-1 (skip-1 _.digit?))));; TODO no leading 0s
                        (feed number<-string))))
           ;; TODO: more
           )))

(make default-builder
  (to (_ .literal string) (lit string))
  (to (_ .keyword string) (then (lit string) word-boundary))
  (to (_ .match regex-string) (surely #no))  ;TODO
  )

(to (parse-grammar text)
  (let outcome (parse parson-grammar text))
  (let skeletons outcome.?results)

  (unless skeletons
    outcome.display (newline)
    (error "Ungrammatical grammar"))

  (let lhses (bag<- (each _.first skeletons)))
  (let duplicates (for where ((n lhses)) (< 1 n)))
  (unless duplicates.empty?
    (error "Multiply-defined rules" (sort duplicates)))

  (let all-refs (union-over (for each ((`(,_ (,refs ,_)) skeletons))
                              refs)))
  (let undefined (all-refs .difference lhses))
  (unless undefined.empty?
    (error "Undefined rules" (sort undefined.keys)))

  skeletons)

(when #no
  (let text "
r: 'yo' z.
r: 'dude'.
s: .
s: r.
t: .
")
  (let skeletons (parse-grammar text))
  (for each! ((`(,name (,refs ,_)) skeletons))
    (format "~d: ~w\n" name refs)))

(export grammar<- parse feed push)
