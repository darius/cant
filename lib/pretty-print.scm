;; Pretty printing
;; Let's start by trying out Wadler's "A prettier printer".

;; Document datatypes
;; (@docs)
;; {line}
;; {text string}
;; {nest n doc}

(to (<> @docs) docs)
(let nil '())
(let line {line})
(to (text str) {text str})
(to (nest n doc) {nest n doc})

(to (lay-out doc)
  (match doc
    ((@docs)
     (let r (foldr chain (each lay-out docs) ""))
     (surely (string? r))
     r)
    ({line}
     "\n")
    ({text str}
     (surely (string? str))
     str)
    ({nest n doc1}
     (let r (lay-out doc1))
     (surely (string? r))
     (r .replace "\n" (chain "\n" (" " .repeat n))))))

(to (pp sexpr)
  (match sexpr
    (((: s symbol?) _ @rest)
     (<> (text "(")
         (nest 1 (<> (text s.name) (text " ")
                     (nest (+ s.name.count 1)
                           (call <> (intersperse line (each pp sexpr.rest))))))
         (text ")")))
    ((: list?)
     (<> (text "(")
         (nest 1 (call <> (intersperse line (each pp sexpr))))
         (text ")")))
    (_ (text ("~w" .format sexpr)))))

(to (intersperse between elements)
  (if elements.empty?
      elements
      `(,elements.first
        ,@(for gather ((x elements.rest))
            `(,between ,x)))))

(let eg1
  '(to (fact n)
     (if (= n 0) 1 (* n (fact (- n 1))))))

(to (main _)
  (display (lay-out (pp eg1)))
  (newline))

(export lay-out)
