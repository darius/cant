;; PEG parsing
;; from https://github.com/darius/parson, but incomplete

;; TODO: fuller error reporting
;; TODO: memoize
;; TODO: delay semantic actions until final success
;; TODO: a top-level fn that raises an error on failure

;; Glossary:
;;  p, q       parsing expression
;;  text       input sequence
;;             (We call it 'text' here even though these operations
;;             don't care what kind of sequence they deal with. It had
;;             better support efficient access by index, though -- I
;;             don't recommend a linked list.)
;;  far        the rightmost index tentatively advanced to in text
;;             (used for error reporting)
;;  i, j       index into text
;;  vals, vs   list of parsed values

;; A parser (i.e. parsing expression) is represented as a function 
;;   p: text far i vals -> result
;; where result is a failure or success object as defined below
;; (or anyway an object following the same protocol as failure/success).

(to (parse parser text)
  (parser text 0 0 '()))

;; The first, basic parsers just fail or succeed right away.

(to (fail text far i vals)
  (make failure
    (to (_ .continue _)     failure)
    (to (_ .else p j vs)    (p text far j vs))
    (to _.invert            empty)
    (to (_ .capture-from _) failure)
    (to (_ .prefix _)       failure)
    (to _.leftovers         (error "Parse failed" failure.postmortem))
    (to _.?results          #no)
    (to _.results           (error "Parse failed" failure.postmortem))
    (to _.result            (error "Parse failed" failure.postmortem))
    (to _.postmortem        `(,(text .slice 0 far) /
                              ,(text .slice far)))
    (to _.display                         ;TODO change to .selfie
      (let `(,left / ,right) failure.postmortem)
      (format "failed: ~w/~w" left right))))

(to (empty text far i vals)
  (make success
    (to (_ .continue p)       (p text far i vals))
    (to (_ .else _ _ _)       success)
    (to _.invert              fail)
    (to (_ .capture-from j)   (empty text far i `(,@vals ,(text .slice j i))))
    (to (_ .prefix pre-vals)  (empty text far i (chain pre-vals vals)))
    (to _.leftovers           i)
    (to _.?results            vals)
    (to _.results             vals)
    (to _.result              vals.maybe)    ;TODO nicer error on wrong # of vals
    (to _.display
      (format "~w ~w" (text .slice i) vals))))

;; (invert p): a parser that succeeds from a state just when p fails
;; from the same state. Any parsing by p becomes mere lookahead; it
;; won't affect any of the result, not even the 'far' point.
(to ((invert p) text far i vals)
  (((p text far i vals) .invert) text far i vals))

;; Return a parser that acts like p, except it appends a value: the
;; text that p matched.
(to ((capture p) text far i vals)
  ((p text far i vals) .capture-from i))

(to ((folded<- combine) @arguments)     ;TODO support arguments.empty?
  (foldr1 combine arguments))

;; (either p1 ... pn): a parser that succeeds just when one of its
;; arguments does, trying them in order.
(let either
  (folded<- (to ((<either> p q) text far i vals)
              ((p text far i vals) .else q i vals))))

;; (then p1 ... pn): a parser that succeeds just when all of p1..pn
;; do, each one starting where the last left off.
(let then
  (folded<- (to ((<then> p q) text far i vals)
              ((p text far i vals) .continue q))))

;; (feed-list f): a parser that replaces the vals with a singleton, (f vals).
(to ((feed-list f) text far i vals)
  (empty text far i `(,(f vals))))

;; The same, but for a multi-argument f.
(to (feed f)
  (feed-list (on (vals) (f @vals))))

;; TODO Python Parson also has alter(f), trace(message), others?

;; Succeed, clearing away any vals.
(to (drop text far i vals)
  (empty text far i '()))

;; Succeed, appending `constant` to the vals.
(to ((push constant) text far i vals)
  (empty text far i `(,@vals ,constant)))

;; (seclude p): parse as p, but where p doesn't get to see or alter
;; the incoming values. If p succeeds, producing new vals, *then*
;; append them to the overall result.
;;
;; When you write a grammar, you typically want to seclude most
;; productions: e.g. a rule A ::= B C | D
;; would be like (seclude (either (then B C (feed a<-bc))
;;                                (then D   (feed a<-d))))
;;
;; But a style that secludes all of your productions would be
;; constraining, like writing a recursive-descent parser without ever
;; passing parameters or keeping loop state. You *could* get along
;; without those features, as long as the data only enters into
;; semantics, not syntactic predicates: instead of taking parameters,
;; return a closure that takes them, and instead of loop state, return
;; a data structure for a later pass in code to deal with. These are
;; pretty annoying hacks to need for things as ordinary as parsing
;; left-associative operators -- which would be just a while-loop in a
;; recursive-descent parser. In Parson it's equally simple: the `many`
;; combinator (below) of a non-secluded loop body. (Call it a left
;; fold if you prefer.)
;;
;; Sometimes you want to seclude only part of a production. `seclude`
;; works together with `feed` to pipe results from producers to
;; consumers.
(to ((seclude p) text far i vals)
  ((p text far i '()) .prefix vals))

;; When (thunk) gets called, it must return a parser p. We don't call
;; it right away; instead (delay thunk) returns a parser that, the
;; first time *it* gets called, calls (thunk) and acts as the
;; resulting p, then and thereafter. Use this for recursive grammars.
;; TODO: implement promises instead
(to (delay thunk)
  (let p (box<- (on (text far i vals)
                  ;; TODO detect left-recursion loops at parse time
                  (p .^= (thunk))
                  (p.^ text far i vals))))
  (on (text far i vals)
    (p.^ text far i vals)))

;; Advance one place if possible -- i.e. we're short of the end, and
;; ok? is true of the element at the current place (normally, the
;; character we're looking at).
(to ((skip-1 ok?) text far i vals)
  (if (and (text .maps? i) (ok? (text i)))
      (empty text (max far i.+) i.+ vals)
      (fail text far i vals)))


;; Derived combinators

;; Like (skip-1 ok?), but appending the one-element sequence (one-char
;; substring) to the vals.
(to (take-1 ok?)
  (capture (skip-1 ok?)))

(to ((always value) _)              ;TODO move to stdlib?
  value)

(let any-1      (take-1 (always #yes)))
(let skip-any-1 (skip-1 (always #yes)))

;; Succeed just at the end of the input.
(let end (invert skip-any-1))

;; E.g. (lit-1 #\x) advances past exactly "x", or fails.
(to (lit-1 my-char)
  (skip-1 (-> (= my-char it)))) ;TODO I think <=> in place of = would catch some bugs in client code

;; E.g. (lit "hello") advances past exactly "hello", or fails.
(to (lit string)
  (foldr then (each lit-1 string) empty))

;; Always succeed, but preferably after p.
(to (maybe p)
  (either p empty))

(make many
  ;; Parse as 0 or more p's in sequence, i.e. Kleene star.
  (to (_ p)
    (let p* (maybe (then p (delay (: p*))))))
  ;; 0 or more p's, with separator in between each.
  (to (_ p separator)
    (maybe (then p (many (then separator p))))))

(make at-least-1
  ;; Parse as 1 or more p's in sequence, i.e. Kleene plus.
  (to (_ p)
    (let p+ (then p (maybe (delay (: p+))))))
  ;; 1 or more p's, with separator in between each.
  (to (_ p separator)
    (then p (many (then separator p)))))

(export invert capture either then feed-list feed push seclude delay
        maybe many at-least-1
        fail empty end skip-1 take-1 any-1 skip-any-1 lit-1 lit drop
        parse)
