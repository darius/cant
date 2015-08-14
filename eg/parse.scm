;; Ported from PAIP chapter 9

(define (parser words)
  "Return all complete parses of a list of words."
  (each '.tree (filter '.complete? (parse words))))

(define (parse words)
  "Return all parses of any prefix of words (working bottom-up)."
  (if (.empty? words)
      '()
      (for each-chained ((rule (lexical-rules (.first words))))
        (extend-parse (.lhs rule) `(,(.first words)) (.rest words) '()))))

(define (extend-parse lhs rhs rest needed)
  "Look for the categories needed to complete the parse."
  (cond ((.empty? needed)
         ;; If nothing needed, return parse and upward extensions.
         (let tree (tree<- lhs rhs))
         (cons (parse<- tree rest)
               (for each-chained ((rule (rules-starting-with lhs)))
                 (extend-parse (.lhs rule) `(,tree)
                               rest (.rest (.rhs rule))))))
        (else
         ;; Otherwise try to extend rightward.
         (for each-chained ((p (parse rest)))
           (if (is? (.lhs p) (.first needed))
               (extend-parse lhs `(,@rhs ,(.tree p))
                             (.remainder p) (.rest needed))
               '())))))

(define (tree<- lhs rhs)
  (make
    (.show () `(,lhs ,@(for each ((part rhs))
                         (if (symbol? part) part (.show part)))))
    (.lhs () lhs)
    (.rhs () rhs)))

(define (rule<- lhs rhs)
  (make
    (.lhs () lhs)
    (.rhs () rhs)
    (.starts-with? (cat)
      (and (list? rhs)
           (not (.empty? rhs))
           (equal? (.first rhs) cat)))))

(define (parse<- tree remainder)
  "A parse tree and a remainder."
  (make
    (.show ()      `((tree: ,(.show tree))
                     (remainder: ,remainder)))
    (.tree ()      tree)
    (.remainder () remainder)
    (.lhs ()       (.lhs tree))
    (.complete? () (.empty? remainder))))

(define (lexical-rules word)
  "Return a list of those rules with word on the rhs."
  (for filter ((rule (*grammar*)))
    (equal? (.rhs rule) word)))

(define (rules-starting-with cat)
  "Return a list of those rules where cat starts the rhs."
  (for filter ((rule (*grammar*)))
    (.starts-with? rule cat)))

(let *grammar* (box<- '()))

(define (grammar<- sexprs)
  (for each ((s sexprs))
    (assert (is? (s 1) '->) "Bad rule syntax" s)
    (assert (= (.count s) 3) "Bad rule syntax" s)
    (rule<- (s 0) (s 2))))


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

(.set! *grammar* grammar3)

(define (try sentence)
  (write sentence) (display ":") (newline)
  (each! print (each '.show (parser sentence)))
  (newline))

(try '(the table))
(try '(the ball hit the table))
(try '(the noun took the verb))
