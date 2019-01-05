;; Norvig's (simpler) spelling corrector
;; http://norvig.com/spell-correct.html
;; TODO: Try imitating https://en.wikibooks.org/wiki/Clojure_Programming/Examples/Norvig_Spelling_Corrector

;; Try to find a word in WORDS that's similar to `word`.
;; Prefer the most common word with the fewest edits.
(to (correct word)
  (or (pick-dictionary-word `(,word))
      (do (let neighbors1 (edits1 word))
          (or (pick-dictionary-word neighbors1)
              (pick-dictionary-word (gather edits1 neighbors1))
              word))))

(to (pick-dictionary-word candidates)
  (let best (max-by WORDS candidates))
  (and (WORDS .maps? best) best))

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
  (chain inserts deletes replaces transposes)) ;N.B. may include duplicates

(let alphabet (each string<- (#\a .to #\z)))

(to (words<-string string)
  ;;  (re:findall "[a-z]+" string.lowercase))  ;TODO
  string.lowercase.split)

(let WORDS
  (bag<- (words<-string (with-input-file '.read-all "eg/spelling.train.text"))))

(each! (compose print correct) (words<-string "a lowsy spelur zzz"))
