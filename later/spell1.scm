;; Norvig's (simpler) spelling corrector
;; http://norvig.com/spell-correct.html
;; TODO: try imitating https://en.wikibooks.org/wiki/Clojure_Programming/Examples/Norvig_Spelling_Corrector
;; TODO: uses pattern matching

(define (correct word)
  (let candidates (or (if-any (known `(,word)))
                      (if-any (known (edits1 word)))
                      (if-any (known-edits2 word))
                      `(,word)))
  (max-by candidates (given (w) (.get NWORDS w 0))))

(define (if-any xs)
  (if (.empty? xs) #no xs))

(define (known words)  ;TODO: iter instead of list? set comprehension?
  (set<-list (for filter ((w words))
               (.has? NWORDS w))))

(define (known-edits2 word)
  (set<-list (for each-chained ((e1 (edits1 word)))
               (for filter ((e2 (edits1 e1)))
                 (.has? NWORDS e2)))))

(define (edits1 word)      ;TODO: real list comprehensions should help
  (let splits     (for each ((i (range<- (+ (.count word) 1))))
                    `(,(.slice word 0 i)
                      ,(.slice word i))))
  (let deletes    (for each (((a b) (for filter (((a b) splits))
                                      (not (.empty? b)))))
                    (chain a (.slice b 1))))
  (let transposes (for each (((a b) (for filter (((a b) splits))
                                     (< 1 (.count b)))))
                    (chain a (string<- (b 1) (b 0)) (.slice b 2))))
  (let replaces   (for each-chained ((a b) splits)
                    (if (.empty? b)
                        '()
                        (for each ((c alphabet))
                          (chain a (string<- c) (.slice b 1))))))
  (let inserts    (for each-chained ((a b) splits)
                    (for each ((c alphabet))
                      (chain a (string<- c) b))))
  (set<-list (chain deletes transposes replaces inserts)))

(let alphabet "abcdefghijklmnopqrstuvwxyz")

(define (train features)
  (let model (map<-))               ;TODO
  (for each! ((f features))
    (.set! model f (+ 1 (.get model f 1))))
  model)

(define (words<-string string)
  (re:findall "[a-z]+" (.lowercase string)))  ;TODO

(let NWORDS (train (words<-string (call-with-open-file "big.txt" '.read-all))))
