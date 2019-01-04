;; Norvig's (simpler) spelling corrector
;; http://norvig.com/spell-correct.html
;; TODO: try imitating https://en.wikibooks.org/wiki/Clojure_Programming/Examples/Norvig_Spelling_Corrector

(to (correct word)
  (let candidates (or (if-any (known (set<- word)))
                      (if-any (known (edits1 word)))
                      (if-any (known-edits2 word))
                      (set<- word)))
  (max-by WORDS candidates.keys))

(to (if-any xs)
  (if xs.empty? #no xs))

(to (known words)
  (words .intersect WORDS))

(to (known-edits2 word)
  (union-over (for each ((e1 ((edits1 word) .keys)))
                (known (edits1 e1)))))

;; TODO real list comprehensions would be nice to have.
(to (edits1 word)
  (let splits     (for each ((i (0 .to word.count)))
                    `(,(word .slice 0 i)
                      ,(word .slice i))))
  (let deletes    (for filter ((`(,a ,b) splits))
                    (and (not b.empty?)
                         (chain a (b .slice 1)))))
  (let transposes (for filter ((`(,a ,b) splits))
                    (and (< 1 b.count)
                         (chain a (string<- (b 1) (b 0)) (b .slice 2)))))
  (let replaces   (for gather ((`(,a ,b) splits))
                    (if b.empty?
                        '()
                        (for each ((c alphabet))
                          (chain a (string<- c) (b .slice 1))))))
  (let inserts    (for gather ((`(,a ,b) splits))
                    (for each ((c alphabet))
                      (chain a (string<- c) b))))
  (set<-list (chain deletes transposes replaces inserts)))

(let alphabet (#\a .to #\z))

(to (words<-string string)
  ;;  (re:findall "[a-z]+" string.lowercase))  ;TODO
  string.lowercase.split)

(let WORDS
  (bag<- (words<-string (with-input-file '.read-all "eg/spelling.train.text"))))

(each! (compose print correct) (words<-string "a lowsy spelur zzz"))
