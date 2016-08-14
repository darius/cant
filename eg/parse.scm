;; Ported from PAIP chapter 9

(define (grammar<-rules rules)

  ;; Return a list of those rules with word on the rhs.
  (define (lexical-rules word)
    (for filter ((rule rules))
      (= rule.rhs word)))

  ;; Return a list of those rules where cat starts the rhs.
  (define (rules-starting-with cat)
    (for filter ((rule rules))
      (rule .starts-with? cat)))

  ;; Look for the categories needed to complete the parse.
  (define (extend-parse lhs rhs rest needed)
    (case (needed.empty?
           ;; Return parse and upward extensions.
           (let tree (tree<- lhs rhs))
           (cons (parse<- tree rest)
                 (for gather ((rule (rules-starting-with lhs)))
                   (extend-parse rule.lhs `(,tree)
                                 rest rule.rhs.rest))))
          (else
           ;; Try to extend rightward.
           (for gather ((p (grammar .parse-prefixes rest)))
             (if (= p.lhs needed.first)
                 (extend-parse lhs `(,@rhs ,p.tree)
                               p.remainder needed.rest)
                 '())))))

  (make grammar

    ;; Return all complete parses of a list of words.
    ({.parse words}
     (each '.tree (filter '.complete? (grammar .parse-prefixes words))))

    ;; Return all parses of any prefix of words (working bottom-up).
    ({.parse-prefixes words}
     (if words.empty?
         '()
         (for gather ((rule (lexical-rules words.first)))
           (extend-parse rule.lhs `(,words.first) words.rest '()))))))

(define (grammar<- sexprs)
  (grammar<-rules (for each (((lhs '-> rhs) sexprs))
                    (rule<- lhs rhs))))

(define (rule<- lhs rhs)
  (make
    ({.lhs} lhs)
    ({.rhs} rhs)
    ({.starts-with? cat}
     (match rhs
       ((first @_) (= first cat))
       (_ #no)))))

(define (tree<- lhs rhs)
  (make
    ({.lhs} lhs)
    ({.rhs} rhs)
    ({.selfie sink} (sink .print (cons lhs rhs)))))

;; A parse tree and a remainder.
(define (parse<- tree remainder)
  (make
    ({.tree}      tree)
    ({.remainder} remainder)
    ({.lhs}       tree.lhs)
    ({.complete?} remainder.empty?)
    ({.selfie sink}
     (sink .print `((tree: ,tree)
                    (remainder: ,remainder))))))


;; Example grammars

(let grammar3
  (grammar<-
   '((Sentence -> (NP VP))
     (NP -> (Art Noun))
     (VP -> (Verb NP))
     (Art -> the) (Art -> a)
     (Noun -> man) (Noun -> ball) (Noun -> woman) (Noun -> table)
     (Noun -> noun) (Noun -> verb)
     (Verb -> hit) (Verb -> took) (Verb -> saw) (Verb -> liked))))

(let grammar4
  (grammar<-
   '((S -> (NP VP))
     (NP -> (D N))
     (NP -> (D A+ N))
     (NP -> (NP PP))
     (NP -> (Pro))
     (NP -> (Name))
     (VP -> (V NP))
     (VP -> (V))
     (VP -> (VP PP))
     (PP -> (P NP))
     (A+ -> (A))
     (A+ -> (A A+))
     (Pro -> I) (Pro -> you) (Pro -> he) (Pro -> she)
     (Pro -> it) (Pro -> me) (Pro -> him) (Pro -> her)
     (Name -> John) (Name -> Mary)
     (A -> big) (A -> little) (A -> old) (A -> young)
     (A -> blue) (A -> green) (A -> orange) (A -> perspicuous)
     (D -> the) (D -> a) (D -> an)
     (N -> man) (N -> ball) (N -> woman) (N -> table) (N -> orange)
     (N -> saw) (N -> saws) (N -> noun) (N -> verb)
     (P -> with) (P -> for) (P -> at) (P -> on) (P -> by) (P -> of) (P -> in)
     (V -> hit) (V -> took) (V -> saw) (V -> liked) (V -> saws))))


;; Smoke test

(define (try grammar sentence)
  (write sentence) (display ":") (newline)
  (each! print (grammar .parse sentence))
  (newline))

(try grammar3 '(the table))
(try grammar3 '(the ball hit the table))
(try grammar3 '(the noun took the verb))

;(try grammar4 '(the man hit the table with the ball))
;(try grammar4 '(the orange saw))
