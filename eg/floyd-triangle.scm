;; https://rosettacode.org/wiki/Floyd%27s_triangle

(to (floyd-triangle n-rows)
  (let rows (flexarray<-))
  (begin counting ((r 1) (i 1))
    (when (<= r n-rows)
      (rows .push! (each string<-number (i .span r)))
      (counting r.+ (+ i r))))
  (let widths (each _.count rows.last))
  (for each! ((row rows))
    (format "~d\n" (" " .join (for each ((`(,col ,string) row.items))
                                (string .right-justify (widths col)))))))

(floyd-triangle 5)
(floyd-triangle 14)
