(import (use "eg/advent-of-code/utils")
  simple-parser<-
  manhattan-distance<-
)

(let input (with-input-file '.read-lines "advent23"))

(let parser
  (simple-parser<- "'pos=<' [:int ',' :int ',' :int :hug] '>, r=' :int"))
(to (parse string)
  ('.results (parser string)))

(let inputs (each parse input))

(let `(,strongest-p ,strongest-r) (max-by inputs
                                          (given (`(,p ,r)) r)))
(let n-near-strongest ('.count (for those ((`(,p ,_) inputs))
                                 (<= (manhattan-distance<- p strongest-p)
                                     strongest-r))))
                        

(display "\nPart 1\n")

(to (part-1)
  n-near-strongest)

(format "~w\n" (part-1))


(display "\nPart 2\n")

(to (part-2)
  'xxx)

(format "~w\n" (part-2))
