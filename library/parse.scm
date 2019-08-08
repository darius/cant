;; Parse with a context-free grammar and a chart.
;; Ported from PAIP chapter 9

(to (grammar<-rules rules)

  ;; Return a list of those rules with word on the rhs.
  (to (lexical-rules word)
    (for those ((rule rules))
      (= rule.rhs word)))

  ;; Return a list of those rules where cat starts the rhs.
  (to (rules-starting-with cat)
    (those (_ .starts-with? cat) rules))

  ;; Look for the categories needed to complete the parse.
  (to (extend-parse lhs rhs rest needed)
    (hm (when needed.empty?
          ;; Return parse and upward extensions.
          (let tree (tree<- lhs rhs))
          (link (parse<- tree rest)
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
    (to (_ .parse words)
      (each _.tree (those _.complete? (grammar .parse-prefixes words))))

    ;; Return all parses of any prefix of words (working bottom-up).
    (to (_ .parse-prefixes words)
      (if words.empty?
          '()
          (for gather ((rule (lexical-rules words.first)))
            (extend-parse rule.lhs `(,words.first) words.rest '()))))))

(to (grammar<- sexprs)
  (grammar<-rules (for each ((`(,lhs -> ,rhs) sexprs))
                    (rule<- lhs rhs))))

(to (rule<- lhs rhs)
  (make _
    (to _.lhs lhs)
    (to _.rhs rhs)
    (to (_ .starts-with? cat)
      (may rhs
        (be `(,(= cat) ,@_) #yes)
        (else               #no)))))

(to (tree<- lhs rhs)
  (make _
    (to _.lhs lhs)
    (to _.rhs rhs)
    (to (_ .selfie sink) (sink .write `(,lhs ,@rhs)))))

;; A parse tree and a remainder.
(to (parse<- tree remainder)
  (make
    (to _.tree      tree)
    (to _.remainder remainder)
    (to _.lhs       tree.lhs)
    (to _.complete? remainder.empty?)
    (to (_ .selfie sink)
      (sink .write `((tree: ,tree)
                     (remainder: ,remainder))))))

(export grammar<-
        grammar<-rules rule<-)
