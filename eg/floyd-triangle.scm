;; https://rosettacode.org/wiki/Floyd%27s_triangle

(to (floyd-triangle n-rows)
  (let rows (begin counting ((r 1) (i 1) (rows '()))
              (if (<= r n-rows)
                  (counting (+ r 1)
                            (+ i r)
                            (link (each string<-number (i .span r))
                                  rows))
                  (reverse rows))))
  (let widths (each '.count rows.last))
  (for each! ((row rows))
    (format "~d\n" (" " .join (for each ((string row)
                                         (width (widths .slice 0 row.count)))
                                (string .right-justify width))))))

(floyd-triangle 5)
(newline)
(floyd-triangle 14)
