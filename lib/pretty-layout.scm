;; Pretty printing
;; From Wadler's "A prettier printer", adapted for strict evaluation.

;; Document datatypes:

;; (@docs)       Concatenation of docs
;; {text s}      Literal string
;; {line i}      (Newline, then i spaces) or 1 space if flattened
;; {union x y}   Any layout described by either of docs x or y.
;;               Invariant: (x, y) are equal after flattening,
;;                          and y is "no flatter".

;; TODO really because of strictness we should make {group x} a
;; basic doc type in place of {union x y}.

(let nil       '())
(to (text str) {text str})
(let line      {line 0})
(make <>
  (to (_ doc) doc)
  (to (_ @docs) docs))

(to (nest i doc)
  (may doc
    (be {text s}    doc)
    (be {line j}    {line (+ i j)})
    (be {union x y} {union (nest i x) (nest i y)})
    (be `(,@docs)   (for each ((d docs))
                      (nest i d)))))

(to (group doc)
  (let flat (flatten doc)) ;XXX this needs to happen in (best) instead, lazily
  (if (= flat doc) doc {union flat doc}))

(to (flatten doc)
  (may doc
    (be {text s}    doc)
    (be {line i}    {text " "})
    (be {union x y} (flatten x))
    (be `(,@docs)   (each flatten docs))))

(to (pretty-print doc width)
  (lay-out width (best width 0 doc)))

;; Return a 'semidetermined' doc: one with no {union...} in the
;; first line, and where {text...} and {line...} appear only in a
;; (@docs) list (which may have no other types of elements).
(to (best w c doc)
  (may doc
    (be {text _}         `(,doc))
    (be {line _}         `(,doc))
    (be '()              doc)
    (be `((,@x) ,@y)     (best w c (chain x y))) ;TODO chain in <> instead?
    (be `({line ,_} ,@_) doc)
    (be `({text ,s} ,@x) `({text ,s} ,@(best w (+ c s.count) x)))
    (be `({union ,x ,y} ,@z)
      (best w c {union `(,x ,@z) `(,y ,@z)}))
    (be {union x y}
     (let best-x (best w c x))
     (if (fits? (- w c) best-x)
         best-x
         (best w c y)))))

(to (fits? w semidetermined-doc)
  (and (<= 0 w) (may semidetermined-doc
                  (be `({text ,s} ,@x)
                    (fits? (- w s.count) x))
                  (else
                    #yes))))

(to (lay-out w semidetermined-doc)
  (may semidetermined-doc
    ('() "")
    (`({line ,i} ,@x)
     (chain "\n"
            (" " .repeat i)
            (lay-out w (best w i x))))
    (`({text ,s} ,@x)
     (chain s (lay-out w x)))))


(export
  pretty-print lay-out best
  nil <> text line nest group)
