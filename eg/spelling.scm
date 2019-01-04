;; Norvig's (simpler) spelling corrector
;; http://norvig.com/spell-correct.html
;; TODO: Try imitating https://en.wikibooks.org/wiki/Clojure_Programming/Examples/Norvig_Spelling_Corrector

;; TODO: While I know performance was never a big priority in this
;; code, it kind of bugs me that WORDS lookups are repeated: first to
;; filter the edits into the candidates, and secondly to rank them. What
;; collections API would make for nice code without that drawback?

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

(to (edits1 word)
  (let splits     (for each ((i (0 .to word.count)))
                    `(,(word .slice 0 i)
                      ,(word .slice i))))
  (let inserts    (for gather ((`(,a ,b) splits))
                    (for each ((c alphabet))
                      (chain a c b))))
  (let del-splits (for those ((`(,a ,b) splits))
                    (not b.empty?)))
  (let deletes    (for each ((`(,a ,b) del-splits))
                    (chain a (b .slice 1))))
  (let replaces   (for gather ((`(,a ,b) del-splits))
                    (for each ((c alphabet))
                      (chain a c (b .slice 1)))))
  (let transposes (for filter ((`(,a ,b) del-splits))
                    (and (< 1 b.count)
                         (chain a (string<- (b 1) (b 0)) (b .slice 2)))))
  (set<-list (chain inserts deletes replaces transposes)))

(let alphabet (each string<- (#\a .to #\z)))

(to (words<-string string)
  ;;  (re:findall "[a-z]+" string.lowercase))  ;TODO
  string.lowercase.split)

(let WORDS
  (bag<- (words<-string (with-input-file '.read-all "eg/spelling.train.text"))))

(each! (compose print correct) (words<-string "a lowsy spelur zzz"))
