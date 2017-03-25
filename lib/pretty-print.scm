;; Pretty printing
;; Let's start by trying out Wadler's "A prettier printer".
;; TODO more efficient (with laziness, etc.)

;; Document datatypes
;; ()
;; {text s doc}     was ((: string?) @doc)
;; {line i doc}     was ((: integer?) @doc)
;; {union doc1 doc2}  Invariant: (doc1, doc2) flatten the same, and doc2 is "no flatter"

(let nil       '())
(to (text str) {text str '()})
(let line      {line 0 '()})
(to (<> @docs) (foldr <>2 docs '()))

(to (<>2 y z)
  (match y
    (()          z)
    ({text s x}  {text s (<>2 x z)})
    ({line i x}  {line i (<>2 x z)})
    ({union w x} {union (<>2 w z) (<>2 x z)})))

(to (nest i doc)
  (match doc
    (()          '())
    ({text s x}  {text s (nest i x)})
    ({line j x}  {line (+ i j) (nest i x)})
    ({union x y} {union (nest i x) (nest i y)})))

(to (group doc)
  (match doc
    (()          '())
    ({text s x}  {text s (group x)})
    ({line _ _}  {union (flatten doc) doc})
    ({union x y} {union (group x) y})))

(to (flatten doc)
  (match doc
    (()          '())
    ({text s x}  {text s (flatten x)})
    ({line _ x}  {text " " (flatten x)})
    ({union x _} (flatten x))))

;; Formatting

(to (pretty-print doc width)
  (lay-out (best width 0 doc)))

(to (best w c doc)
  (match doc
    (()          '())
    ({text s x}  {text s (best w (+ c s.count) x)})
    ({line i x}  {line i (best w i x)})
    ({union x y}
     (let best-x (best w c x))
     (if (fits? (- w c) best-x)
         best-x
         (best w c y)))))

(to (fits? w doc)
  (and (<= 0 w) (match doc
                  ({text s x} (fits? (- w s.count) x))
                  (_ #yes))))

(to (lay-out doc)
  (match doc
    (()         "")
    ({text s x} (chain s (lay-out x)))
    ({line i x} (chain "\n" (" " .repeat i) (lay-out x)))))

;; Pretty-printing S-expressions

(to (pp sexpr)
  (match sexpr
    (((: first symbol?) _ @_)
     (group (<> (text "(")
                (nest 1 (<> (text first.name) (text " ")
                            (nest (+ first.name.count 1)
                                  (call <> (intercalate line (each pp sexpr.rest))))))
                (text ")"))))
    ((: list?)
     (group (<> (text "(")
                (nest 1 (call <> (intercalate line (each pp sexpr))))
                (text ")"))))
    (_ (text ("~w" .format sexpr)))))

(to (intercalate between elements)      ;TODO unify with .join in stdlib
  (if elements.empty?
      elements
      `(,elements.first
        ,@(for gather ((x elements.rest)) ;TODO more efficient
            `(,between ,x)))))

;; Smoke test

(let eg1
  '(to (fact n)
     (if (= n 0) 1 (* n (fact (- n 1))))))

(to (main _)
  (display (pretty-print (pp eg1) 30))
  (newline))

(export
  lay-out nil <> text line nest group
  pp)
