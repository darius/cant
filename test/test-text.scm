(import (use "lib/text") text<-)

(hide
  (let t (text<-))
  (print (t .get 0 10))
  (t .replace 0 0 "hello")
  (print (t .get 0 10))
  (t .replace 1 2 "GOOBER")
  (print (t .get 0 20))
  (print (t .get 3 3))
  )

