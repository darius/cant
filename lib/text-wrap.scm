;; Like Python's textwrap.
;; TODO try using Parson
;; TODO the Python names are pretty arbitrary

(to (fill text width)
  ("\n" .join (wrap text width)))

(to (wrap text width)
  (surely (< 0 width))  ;TODO 'require' or something, for preconditions
  (wrap-into (fillvector<-) (parse-tokens text) width))

(to (flush buffer)
  (string<-list (as-list buffer))) ;XXX clumsy

(to (parse-tokens text)
  (if text.empty?
      '()
      (do (match text.first
            (#\newline `({break} ,@(parse-tokens text.rest)))
            (#\space   `({space} ,@(parse-tokens text.rest)))
            ((: '.whitespace?)
             (error "I don't know how to fill whitespace like" ch))
            (_ (let word (fillvector<- text.first))
               (begin eating ((text text.rest))
                 (if (or text.empty? text.first.whitespace?)
                     `({word ,(flush word)} ,@(parse-tokens text))
                     (do (word .push! text.first)
                         (eating text.rest)))))))))

(to (wrap-into line tokens width)
  (begin scanning ((spaces 0) (tokens tokens))
    (if tokens.empty?
        (if line.empty? '() `(,(flush line)))
        (match tokens.first
          ({break}
           (cons (flush line) (wrap-into (fillvector<-) tokens.rest width)))
          ({space}
           (scanning (+ spaces 1) tokens.rest))
          ({word s}
           (if (<= (+ line.count spaces s.count) width)
               (do (line .extend! (chain (" " .repeat spaces) s))
                   (scanning 0 tokens.rest))
               (cons (flush line)
                     (hide
                       (let new-line (fillvector<-))
                       (new-line .extend! s)
                       (wrap-into new-line tokens.rest width)))))))))


(to (main (_ width-str @words))         ;just for a quick test
  (let lines (wrap (" " .join words)
                   (number<-string width-str)))
  (for each! ((line lines))
    (format "~w\n" line)))

(export fill wrap)
