;; Pretty printing
;; Let's start by trying out Wadler's "A prettier printer".

;; Document datatypes
;; ()
;; (string @doc)
;; (int @doc)

(let nil       '())
(to (text str) `(,str))
(let line      '(0))
(to (<> @docs) (call chain docs))

(to (nest i doc)
  (match doc
    (()                     '())
    (((: s string?) @rest)  `(,s ,@(nest i rest)))
    (((: j integer?) @rest) `(,(+ i j) ,@(nest i rest)))))

(to (lay-out doc)
  (match doc
    (() "")
    (((: s string?) @rest)
     (chain s (lay-out rest)))
    (((: i integer?) @rest)
     (chain "\n" (" " .repeat i) (lay-out rest)))))

(to (pp sexpr)
  (match sexpr
    (((: s symbol?) _ @rest)
     (<> (text "(")
         (nest 1 (<> (text s.name) (text " ")
                     (nest (+ s.name.count 1)
                           (call <> (intercalate line (each pp sexpr.rest))))))
         (text ")")))
    ((: list?)
     (<> (text "(")
         (nest 1 (call <> (intercalate line (each pp sexpr))))
         (text ")")))
    (_ (text ("~w" .format sexpr)))))

(to (intercalate between elements)      ;TODO unify with .join in stdlib
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
