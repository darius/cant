;; Format a sequence of strings in columns.
;; Based on github.com/darius/columnize

;; Given a list of rows, each a list of strings, return a list of lines.
;; Each line is the corresponding row joined by `spacer`, with each
;; column padded to its max width.
(to (format-table rows @(optional ?spacer ?justify))
  (let spacer  (or ?spacer  " "))
  (let justify (or ?justify ('.left-justify .method)))
  (surely (hide (let lengths (each _.count rows))
                (<= lengths.range.count 1))
          "Rows must be the same length" rows)
  (let widths (for each ((column (transpose rows)))
                (max @(each _.count column))))
  (for each ((row rows))
    (spacer .join (each justify row widths))))

;; Given a sequence of strings, return a matrix of the same strings in
;; column order, trying to fit them in the given width.
(to (tabulate strings @(optional ?width))
  (let width (or ?width 79))
  (let max-width (+ 2 (max 0 @(each _.count strings))))
  (let n-cols (max 1 (min strings.count (width .quotient max-width))))
  (let n-rows (max 1 ((+ strings.count n-cols -1) .quotient n-cols)))
  (let padded (chain strings
                     ('("") .repeat (- (* n-rows n-cols) strings.count))))
  (transpose (for each ((i (range<- 0 strings.count n-rows))) ;TODO `chunk` function?
               (padded .slice i (+ i n-rows)))))

(export format-table tabulate)
