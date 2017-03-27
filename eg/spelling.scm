;; Norvig's (simpler) spelling corrector
;; http://norvig.com/spell-correct.html
;; TODO: try imitating https://en.wikibooks.org/wiki/Clojure_Programming/Examples/Norvig_Spelling_Corrector

(to (correct word)
  (let candidates (or (if-any (known (set<- word)))
                      (if-any (known (edits1 word)))
                      (if-any (known-edits2 word))
                      (set<- word)))
  (arg-max candidates.keys (given (w) (NWORDS .get w 0))))

(to (if-any xs)
  (if xs.empty? #no xs))

;; TODO: NWORDS.keys should be a set, which we just intersect with words.
(to (known words)  ;TODO: iter instead of list? set comprehension?
  (call set<- (for those ((w words.keys))
                (NWORDS .maps? w))))

(to (known-edits2 word)
  (call set<- (for gather ((e1 ((edits1 word) .keys))) ;TODO unugh
                (for those ((e2 ((edits1 e1) .keys)))
                  (NWORDS .maps? e2)))))

;; TODO real list comprehensions would be nice to have.
(to (edits1 word)
  (let splits     (for each ((i (range<- (+ word.count 1))))
                    `(,(word .slice 0 i)
                      ,(word .slice i))))
  (let deletes    (for filter (((a b) splits))
                    (and (not b.empty?)
                         (chain a (b .slice 1)))))
  (let transposes (for filter (((a b) splits))
                    (and (< 1 b.count)
                         (chain a (string<- (b 1) (b 0)) (b .slice 2)))))
  (let replaces   (for gather (((a b) splits))
                    (if b.empty?
                        '()
                        (for each ((c alphabet))
                          (chain a (string<- c) (b .slice 1))))))
  (let inserts    (for gather (((a b) splits))
                    (for each ((c alphabet))
                      (chain a (string<- c) b))))
  (call set<- (chain deletes transposes replaces inserts)))

(let alphabet "abcdefghijklmnopqrstuvwxyz")

(to (train features)
  (let model (map<-))
  (for each! ((f features))
    (model .set! f (+ 1 (model .get f 1))))
  model)

(to (words<-string string)
  ;;  (re:findall "[a-z]+" string.lowercase))  ;TODO
  string.lowercase.split)

(let NWORDS
  (train (words<-string (with-input-file '.read-all "eg/spelling.train.text"))))

(each! (compose print correct) (words<-string "a lowsy spelur zzz"))
