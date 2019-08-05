;; Just imitating a Ruby example from (iirc) _Seven languages in seven weeks_.
;; Represent a CSV file as an object which responds to the column names as getters.

(to (csv<-file filename)
  (csv<-lines (with-input-file _.read-lines filename)))

(to (csv<-lines lines)
  (table<- (parse-csv-line lines.first)
           (each parse-csv-line lines.rest)))

(to (parse-csv-line line)
  (line .split ","))                    ;TODO quoting

(to (table<- header rows)
  (let columns (_.inverse (for each ((column header))
                            (symbol<- (chain "." column)))))
  (to (column? x) (columns .maps? x))
  (to (row<- entries)
    (make row
      (to `{,(? column? tag)}    ;; TODO there should be a (term<- (? column? tag) '()) type of pattern
        (entries (columns tag))) ;; A bit clumsy, looking the tag up twice...
      (to message
        (call entries message))))
  (make table
    (to _.rows
      (each row<- rows))))


;; Example

(let csv (csv<-lines '("one,two"
                       "lion,tiger")))

(print csv)
(print csv.rows)
(print csv.rows.first.two)
(print csv.rows.first.one)
