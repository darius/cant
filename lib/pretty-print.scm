;; Pretty printing
;; Let's start by trying out Wadler's "A prettier printer".

;; Document datatypes
;; (@docs)
;; {line}
;; {text string}
;; {nest n doc}

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

;; OK, so this is gonna suck.
(to (pp sexpr)
  (match sexpr
    (()
     {text "()"})
    (((: s symbol?) sx2 @rest)
     {nest 1 `({text "("} {text ,s.name} {text " "}
               {nest ,(+ s.name.count 1)
                     (,(pp sx2)
                      ,(for each ((sx rest))
                         `({line} ,(pp sx))))}
               {text ")"})})
    ((first @rest)
     {nest 1 `({text "("}
               ,(pp first)
               ,(for each ((arg rest))
                  `({line} ,(pp arg)))
               {text ")"})})
    (_ {text ("~w" .format sexpr)})))
;; Yup, that was ugly.

(let eg1
  '(to (fact n)
     (if (= n 0) 1 (* n (fact (- n 1))))))

(to (main _)
  (display (lay-out (pp eg1)))
  (newline))

(export lay-out)
