;; Parson's concrete language-independent syntax
;; XXX untested
;; XXX leaving out regexes, fnord, anonymous start
;; XXX need to parameterize by semantics

(import (use "lib/bag")
  bag<-)
(import (use "lib/hashset")
  union-over)
(import (use "lib/parson")
  invert capture either then feed-list feed push seclude delay
  maybe many at-least-1
  fail empty end skip-1 take-1 any-1 skip-any-1 lit-1 lit
  parse)

(let hug (feed-list identity))

(to (rule-ref<- name)
  (list<- (set<- name)
          (given (_ rules _)
            (delay (given () (rules name))))))

(to (constant<- p)
  (list<- (set<-)
          (given (_ _ _) p)))

(to (lift peg-op)
  (feed-list
   (given (lifted)
     (list<- (union-over (for each (((refs _) lifted))
                           refs))
             (given (builder rules subs)
               (call peg-op (for each (((_ f) lifted))
                              (f builder rules subs))))))))

(to (literal<- string)
  (list<- (set<-)
          (given (builder _ _) (builder .literal string))))

(to (keyword<- string)
  (list<- (set<-)
          (given (builder _ _) (builder .keyword string))))

(to (unquote<- name)
  (list<- (set<-)
          (given (_ _ subs) (subs name))))

(to (push-lit<- string)
  (constant<- (push string)))

(to (name-char? ch) (or ch.letter? (= ch #\_)))
(to (word-char? ch) (or ch.letter? (= ch #\_))) ;XXX I think this should be broader

(let eat-line
  (delay (given ()
           (either (lit-1 #\newline)
                   (then skip-any-1 eat-line)
                   empty))))

(let whitespace
  (at-least-1 (either (skip-1 '.whitespace?)
                      (then (lit-1 #\#) eat-line))))

(let __ (maybe whitespace))

(let name 
  (then (capture (then (skip-1 name-char?)
                       (many (skip-1 word-char?))))
        __))

(let word
  (then (capture (many (skip-1 word-char?)))
        __))

(to (string-quoted-by q-char)
  (let q (lit-1 q-char))
  (let quoted-char
    (then (either (lit-1 #\\) (invert q))
          any-1))
  (seclude
   (then q (many quoted-char) q __
         (feed-list chain))))           ;XXX if empty, you'll get () instead of ""

(let qstring  (string-quoted-by #\'))
(let dqstring (string-quoted-by #\"))

(let pe
  (delay (given ()
           (seclude
            (either (then term (maybe (then (lit "|") __ pe (lift either))))
                    (lift (given () empty)))))))

(let term
  (delay (given ()
           (seclude
            (then factor (maybe (then term (lift chain))))))))

(let factor
  (delay (given ()
           (seclude
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

(let grammar
  (then __ (at-least-1 rule) end))

(to (grammar<- text)
  (let skeletons (parse-grammar text))
  (let builder default-builder)         ;TODO parameterize
  (given (subs)
    (let rules (map<-a-list (for each (((name (refs f)) skeletons)) ;XXX better name than f
                              `(,name ,(delay (given () (rules name)))))))
    (for each! (((name (refs f)) skeletons))
      (let peg (f builder rules subs))
      (rules .set! name peg))
    rules))

(make default-builder
  ({.literal string} (lit string))
  ({.keyword string} (lit string))      ;XXX then word_boundary
  ({.match regex-string} (surely #no))  ;TODO
  )

(to (parse-grammar text)
  (let outcome (parse grammar text))
  (let skeletons outcome.opt-results)
  (unless skeletons
    outcome.display (newline)
    (error "Ungrammatical grammar"))
  (let lhses (each '.first skeletons))
  (let all-refs (union-over (for each (((_ (refs _)) skeletons))
                              refs)))
  (let undefined (all-refs .difference (call set<- lhses)))
  (unless undefined.empty?
    (error "Undefined rules" (sort undefined.keys)))
  (let counts (call bag<- lhses))
  (let dups (for filter (((lhs n) counts.items))
              (< 1 n)))
  (unless dups.empty?
    (error "Multiply-defined rules" (sort dups)))
  skeletons)

(to (main _)                            ;smoke test
  (let text "
main: r*.
r: .
s: 'hey' r.
")
  (let skeletons (parse-grammar text))
  (for each! (((name (refs semantics)) skeletons))
    (format "%d: %w\n" name refs))
  (let defns (map<-a-list skeletons))
  )

(export grammar)
