;; Parser

(import (use "lib/parson")
  parse delay seclude either then invert feed maybe many
  empty lit-1 any-1 skip-any-1)
(import (use "lib/regex-match")
  lit<- alt<- chain<- star<-)

(let regex-parser
  (hide
   (let primary (delay (to (<primary>)
                         (seclude
                          (either (then (lit-1 #\() exp (lit-1 #\)))
                                  (then (invert (either (lit-1 #\))
                                                        (lit-1 #\|)
                                                        (lit-1 #\*)))
                                        any-1
                                        (feed (given (s) (lit<- (s 0))))))))))
   (let factor (seclude
                (then primary
                      (maybe (either (then (lit-1 #\*)
                                           (feed star<-)))))))
   (let term (delay (to (<term>)
                      (seclude
                       (then factor (many (then term
                                                (feed chain<-))))))))
   (let exp (delay (to (<exp>)
                     (seclude
                      (either (then term (many (then (lit-1 #\|) exp
                                                     (feed alt<-))))
                              empty)))))
   (then exp (invert skip-any-1))))

(to (parse-regex string)
  ((parse regex-parser string) .result))


(export parse-regex regex-parser)
