;; https://rosettacode.org/wiki/Floyd%27s_triangle

(to (floyd-triangle n-rows)
  (let rows (flexarray<-))
  (begin counting ((r 1) (i 1))
    (when (<= r n-rows)
      (rows .push! (each string<-number (i .span r)))
      (counting (+ r 1) (+ i r))))
  (let widths (each '.count rows.last))
  (for each! ((row rows))
    (format "~d\n" (" " .join (for each ((`(,c ,string) row.items))
                                (string .right-justify (widths c)))))))

(floyd-triangle 5)
(floyd-triangle 14)
