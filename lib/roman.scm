;; Roman numerals

(let values
  (map<- '((#\M 1000) (#\D 500) (#\C 100) (#\L 50) (#\X 10) (#\V 5) (#\I 1))))

;; TODO: expose as a parson grammar, too?
(to (int<-roman str)
  (let `(,_ ,total)
    (for foldr ((digit (each values str.uppercase))
                (`(,next ,total) '(0 0)))
      `(,digit ,((if (<= next digit) + -) total digit))))
  total)

(let digits (" I II III IV V VI VII VIII IX" .split " "))
(let places (map<- (zip "IVXLC"
                        "XLCDM")))

(to (roman<-int n)
  (let `(,tens ,ones) (n ./mod 10))
  (chain (match tens
           (0 "")
           (_ (times-X (roman<-int tens))))
         (digits ones)))

(to (times-X numeral)
  (call string<- (each places numeral)))


(to (main args)
  (for each! ((x args.rest))
    (let converted (if (every '.digit? x) ;TODO string method? it's there in parson, at least.
                       (roman<-int (number<-string x))
                       (int<-roman x)))
    (format "~d: ~d\n" x converted)))

(export int<-roman roman<-int main)
