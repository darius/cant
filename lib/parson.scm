;; PEG parsing
;; from https://github.com/darius/parson, but incomplete

;; TODO: fuller error reporting
;; TODO: memoize
;; TODO: delay semantic actions until final success

;; Glossary:
;;  p, q       parsing expression
;;  text       input sequence
;;  far        the rightmost index tentatively eaten up to in text
;;             (used for error reporting)
;;  i, j       index into text
;;  vals, vs   list of parsed values

(define (parse parser text)
  (parser text 0 0 '()))

(define (fail text far i vals)
  (make failure
    ({.display}                         ;TODO change to .selfie
     (display "failed: ")
     (write (text .slice 0 far))
     (display "/")
     (write (text .slice far)))
    ({.invert}           empty)
    ({.else p text j vs} (p text far j vs))
    ({.continue p}       failure)
    ({.capture-from j}   failure)
    ({.prefix pre-vals}  failure)
    ({.leftovers}        (error "Parsing failed" failure))
    ({.opt-results}      #no)
    ({.result}           (error "Parsing failed" failure))))

(define (empty text far i vals)
  (make success
    ({.display} 
     (write (text .slice i))
     (display " ")
     (write vals))
    ({.invert}           fail)
    ({.else p text j vs} success)
    ({.continue p}       (p text far i vals))
    ({.capture-from j}   (empty text far i `(,@vals ,(text .slice j i))))
    ({.prefix pre-vals}  (empty text far i `(,@pre-vals ,@vals)))
    ({.leftovers}        i)
    ({.opt-results}      vals)
    ({.result}
     (if (= 1 vals.count)
         vals.first
         (error "Wrong # of results" vals)))))

(define ((invert p) text far i vals)
  (let p-result (p text far i vals))
  (p-result.invert text far i vals))

(define ((capture p) text far i vals)
  (let p-result (p text far i vals))
  (p-result .capture-from i))

(define ((folded<- combine) @arguments)
  (foldr1 combine arguments))

(let either
  (folded<- (define ((<either> p q) text far i vals)
              ((p text far i vals) .else q text i vals))))

(let then
  (folded<- (define ((<then> p q) text far i vals)
              ((p text far i vals) .continue q))))

(define ((feed-list f) text far i vals)
  (empty text far i `(,(f vals))))

(define (feed f)
  (feed-list (given (vals) (call f vals))))

(define ((push constant) text far i vals)
  (empty text far i `(,@vals ,constant)))

(define ((seclude p) text far i vals)
  ((p text far i '()) .prefix vals))

;;TODO: implement promises instead
(define (delay thunk)
  (let p (box<- (given (text far i vals)
                  (p .^= (thunk))
                  (p.^ text far i vals))))
  (given (text far i vals)
    (p.^ text far i vals)))

(define ((skip-1 ok?) text far i vals)
  (if (and (text .maps? i) (ok? (text i)))
      (empty text (max far (+ i 1)) (+ i 1) vals)
      (fail text far i vals)))


;; Derived combinators

(define (take-1 ok?)
  (capture (skip-1 ok?)))

(define ((always value) _)              ;TODO move to stdlib?
  value)

(let any-1      (take-1 (always #yes)))
(let skip-any-1 (skip-1 (always #yes)))

(define (lit-1 my-char)
  (skip-1 (given (char) (= my-char char))))

(define (lit string)
  (foldr then (each lit-1 string) empty))

(define (maybe p)
  (either p empty))

(make many
  ((p)
   (let p* (maybe (then p (delay (given () p*))))))
  ((p separator)
   (maybe (then p (many (then separator p))))))

(make at-least-1
  ((p)
   (let p+ (then p (maybe (delay (given () p+))))))
  ((p separator)
   (then p (many (then separator p)))))

(export invert capture either then feed-list feed push seclude delay maybe many at-least-1
        fail empty skip-1 take-1 any-1 skip-any-1 lit-1 lit
        parse)
