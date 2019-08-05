;; PEG parsing
;; from https://github.com/darius/parson, but incomplete

;; TODO: fuller error reporting
;; TODO: memoize
;; TODO: delay semantic actions until final success
;; TODO: a top-level fn that raises an error on failure

;; Glossary:
;;  p, q       parsing expression
;;  text       input sequence
;;  far        the rightmost index tentatively eaten up to in text
;;             (used for error reporting)
;;  i, j       index into text
;;  vals, vs   list of parsed values

(to (parse parser text)
  (parser text 0 0 '()))

(to (fail text far i vals)
  (make failure
    (to (_ .continue _)     failure)
    (to (_ .else p j vs)    (p text far j vs))
    (to _.invert            empty)
    (to (_ .capture-from _) failure)
    (to (_ .prefix _)       failure)
    (to _.leftovers         (error "Parse failed" failure.postmortem))
    (to _.opt-results       #no)
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
    (to (_ .prefix pre-vals)  (empty text far i `(,@pre-vals ,@vals)))
    (to _.leftovers           i)
    (to _.opt-results         vals)
    (to _.results             vals)
    (to _.result              vals.maybe)    ;TODO nicer error on wrong # of vals
    (to _.display
      (write (text .slice i))
      (display " ")
      (write vals))))

(to ((invert p) text far i vals)
  (((p text far i vals) .invert) text far i vals))

(to ((capture p) text far i vals)
  ((p text far i vals) .capture-from i))

(to ((folded<- combine) @arguments)     ;TODO support arguments.empty?
  (foldr1 combine arguments))

(let either
  (folded<- (to ((<either> p q) text far i vals)
              ((p text far i vals) .else q i vals))))

(let then
  (folded<- (to ((<then> p q) text far i vals)
              ((p text far i vals) .continue q))))

(to ((feed-list f) text far i vals)
  (empty text far i `(,(f vals))))

(to (feed f)
  (feed-list (on (vals) (call f vals))))

(to (drop text far i vals)
  (empty text far i '()))

(to ((push constant) text far i vals)
  (empty text far i `(,@vals ,constant)))

(to ((seclude p) text far i vals)
  ((p text far i '()) .prefix vals))

;;TODO: implement promises instead
(to (delay thunk)
  (let p (box<- (on (text far i vals)
                  (p .^= (thunk))
                  (p.^ text far i vals))))
  (on (text far i vals)
    (p.^ text far i vals)))

(to ((skip-1 ok?) text far i vals)
  (if (and (text .maps? i) (ok? (text i)))
      (empty text (max far (+ i 1)) (+ i 1) vals)
      (fail text far i vals)))


;; Derived combinators

(to (take-1 ok?)
  (capture (skip-1 ok?)))

(to ((always value) _)              ;TODO move to stdlib?
  value)

(let any-1      (take-1 (always #yes)))
(let skip-any-1 (skip-1 (always #yes)))

(let end (invert skip-any-1))

(to (lit-1 my-char)
  (skip-1 (-> (= my-char it))))

(to (lit string)
  (foldr then (each lit-1 string) empty))

(to (maybe p)
  (either p empty))

(make many
  (`(,p)
   (let p* (maybe (then p (delay (: p*))))))
  (`(,p ,separator)
   (maybe (then p (many (then separator p))))))

(make at-least-1
  (`(,p)
   (let p+ (then p (maybe (delay (: p+))))))
  (`(,p ,separator)
   (then p (many (then separator p)))))

(export invert capture either then feed-list feed push seclude delay
        maybe many at-least-1
        fail empty end skip-1 take-1 any-1 skip-any-1 lit-1 lit drop
        parse)
