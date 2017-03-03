;; Norvig's (simpler) spelling corrector
;; http://norvig.com/spell-correct.html
;; TODO: try imitating https://en.wikibooks.org/wiki/Clojure_Programming/Examples/Norvig_Spelling_Corrector

(define (correct word)
  (let candidates (or (if-any (known (set<- word)))
                      (if-any (known (edits1 word)))
                      (if-any (known-edits2 word))
                      (set<- word)))
  (arg-max candidates.keys (given (w) (NWORDS .get w 0))))

(define (if-any xs)
  (if xs.empty? #no xs))

;; TODO: NWORDS.keys should be a set, which we just intersect with words.
(define (known words)  ;TODO: iter instead of list? set comprehension?
  (call set<- (for filter ((w words.keys))
                (NWORDS .maps? w))))

(define (known-edits2 word)
  (call set<- (for gather ((e1 ((edits1 word) .keys))) ;TODO unugh
                (for filter ((e2 ((edits1 e1) .keys)))
                  (NWORDS .maps? e2)))))

;; TODO real list comprehensions would be nice to have.
(define (edits1 word)
  (let splits     (for each ((i (range<- (+ word.count 1))))
                    `(,(word .slice 0 i)
                      ,(word .slice i))))
  (let deletes    (for each (((a b) (for filter (((a b) splits))
                                      (not b.empty?))))
                    (chain a (b .slice 1))))
  (let transposes (for each (((a b) (for filter (((a b) splits))
                                      (< 1 b.count))))
                    (chain a (string<- (b 1) (b 0)) (b .slice 2))))
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

(define (train features)
  (let model (map<-))
  (for each! ((f features))
    (model .set! f (+ 1 (model .get f 1))))
  model)

(define (words<-string string)
  ;;  (re:findall "[a-z]+" string.lowercase))  ;TODO
  string.lowercase.split)

(let NWORDS
  (train (words<-string (for with-input-file ((source "eg/spelling.train.text"))
                          source.read-all))))

(each! (compose print correct) (words<-string "a lowsy spelur zzz"))
