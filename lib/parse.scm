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
