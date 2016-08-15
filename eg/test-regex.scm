; TODO more tests

(hide
 (let r ((regex-parser "a(b|c)*d" 0 0 '()) .result))

 (print (re-match r "ad"))
 (print (re-match r "ab"))
 (print (re-match r "abcd"))
 (print (re-match r "B"))
)
