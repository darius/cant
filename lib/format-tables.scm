;; Format a sequence of strings in columns.
;; Based on github.com/darius/columnize

;; Given a list of rows, each a list of strings, return a list of lines.
;; Each line is the corresponding row joined by `spacer`, with each
;; column padded to its max width.
(to (format-table rows @(optional opt-spacer opt-justify))
  (let spacer  (or opt-spacer  " "))
  (let justify (or opt-justify '.left-justify))
  (surely (for every ((row rows))
            (= row.count ((rows 0) .count))))
  (let widths (for each ((column (transpose rows)))
                (call max (each '.count column))))
  (for each ((row rows))
    (spacer .join (for each ((pair (zip row widths)))
                    (call justify pair)))))

;; Given a sequence of strings, return a matrix of the same strings in
;; column order, trying to fit them in the given width.
(to (tabulate strings @(optional opt-width))
  (let width (or opt-width 79))
  (let max-width (+ 2 (call max `(0 ,@(each '.count strings)))))
  (let n-cols (max 1 (min strings.count (width .quotient max-width))))
  (let n-rows (max 1 ((+ strings.count n-cols -1) .quotient n-cols)))
  (let padded (chain strings
                     ('("") .repeat (- (* n-rows n-cols) strings.count))))
  (transpose (for each ((i (range<- 0 strings.count n-rows))) ;XXX
               (padded .slice i (+ i n-rows)))))

(export format-table tabulate)
