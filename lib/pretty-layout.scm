;; Pretty printing
;; Let's start by trying out Wadler's "A prettier printer".
;; TODO more efficient (with laziness, etc.)

;; Document datatypes:
;; ()                 Empty document
;; {text s doc}       String s followed by doc
;; {line i doc}       (Newline, then i spaces) or 1 space if flattened; then doc
;; {union doc1 doc2}  Any layout described by either doc1 or doc2.
;;                    Invariant: (doc1, doc2) are equal after flattening,
;;                               and doc2 is "no flatter".

(let nil       '())
(to (text str) {text str '()})
(let line      {line 0 '()})
(to (<> @docs) (foldr <>2 docs '()))

(to (<>2 y z)
  (match y
    (()          z)
    ({text s x}  (text<> s (<>2 x z)))
    ({line i x}  {line i (<>2 x z)})
    ({union w x} {union (<>2 w z) (<>2 x z)})))

(to (text<> s x) ; Just a performance hack for {text s x}; maybe not worthwhile.
  (match x
    ({text t y} {text (chain s t) y})
    (_          {text s x})))

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
    ({text s x}  (text<> s (flatten x)))
    ({line _ x}  (text<> " " (flatten x)))
    ({union x _} (flatten x))))

;; Formatting

(to (pretty-print doc @opt-width)
  (let width (match opt-width (() 72) ((w) w)))
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


(export
  pretty-print lay-out
  nil <> text line nest group)
