(import (use "lib/text") text<- backward)

(hide
  (let t (text<-))
  (print (t .get 0 10))
  (t .replace 0 0 "hello")
  (print (t .get 0 10))
  (t .replace 1 2 "GOOBER")
  (print (t .get 0 20))
  (print (t .get 3 3))
  )

(newline)
(hide
  (let t (text<-))
  (t .replace 0 0 "aXbXc")
  (let X (set<- #\X))
  (format "~d\n" (t .get 0 t.count))
  (each! display t.keys)
  (newline)
  (for each! ((i (0 .to t.count)))
    (format "back from ~w: ~w\n"
            i (t .find-char-set i backward X)))
  )
