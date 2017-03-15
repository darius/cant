;; Like Python's textwrap.
;; TODO try using Parson
;; XXX output lines include trailing spaces

(to (fill text width)
  ("\n" .join (wrap text width)))

(to (wrap text width)
  (surely (< 0 width))  ;TODO 'require' or something, for preconditions

  (to (wrapping text)
    (if text.empty?
        '()
        (start-line (fillvector<-) text.first text.rest)))

  (to (start-line line ch rest)

    (to (end-line)
      (string<-list (as-list line))) ;XXX clumsy

    (to (eating ch rest)
      (case ((= ch #\newline)
             (cons (end-line) (wrapping rest)))
            (ch.whitespace?
             (line .push! ch)
             (if (= width line.count)
                 (cons (end-line) (wrapping rest))
                 (if rest.empty?   ;XXX ugly!
                     (cons (end-line) '())
                     (eating rest.first rest.rest))))
            (else
             (start-word ch rest))))

    (to (start-word ch rest)
      (let word (fillvector<- ch))
      (let limit (- width line.count))
      (begin nibbling ((rest rest))
        (case ((or rest.empty? rest.first.whitespace?)
               (case ((<= word.count limit)
                      (line .extend! word)
                      (if rest.empty?   ;XXX ugly!
                          (cons (end-line) '())
                          (eating rest.first rest.rest)))
                     (else
                      (cons (end-line)
                            (if rest.empty?   ;XXX ugly!
                                `(,(string<-list (as-list word)))
                                (start-line word rest.first rest.rest))))))
              (else
               (word .push! rest.first)
               (nibbling rest.rest)))))

    (eating ch rest))

  (wrapping text))

(to (main args)                     ;just for a quick test
  (print (wrap (" " .join args) 40)))

(export fill wrap)
