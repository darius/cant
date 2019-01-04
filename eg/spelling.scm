;; Norvig's (simpler) spelling corrector
;; http://norvig.com/spell-correct.html
;; TODO: try imitating https://en.wikibooks.org/wiki/Clojure_Programming/Examples/Norvig_Spelling_Corrector

;; Try to find a word in WORDS that's similar to `word`. Prefer the most common.
(to (correct word)
  (if (WORDS .maps? word)
      word
      (match (candidates<- word)
        ((? '.empty?) word)
        (candidates (max-by WORDS candidates.keys)))))

;; Edits of `word`, within distance 1 or 2, which are known in WORDS.
(to (candidates<- word)
  (let neighbors (edits1 word))
  (match (known neighbors)
    ((? '.empty?) (union-over (for each ((e1 neighbors.keys))
                                (known (edits1 e1)))))
    (candidates candidates)))

(to (known words)
  (words .intersect WORDS))

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
