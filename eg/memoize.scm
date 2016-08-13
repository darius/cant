(make not-yet)

(define (memoize f)
  (let memos (map<-))
  (define (memoized @arguments)
    (let value (memos .get arguments not-yet))
    (if (= value not-yet)
        (do (let computed (call f arguments))
            (memos .set! arguments computed)
            computed)
        value)))

(hide
 (let fib (memoize
           (given (n)
             (if (< n 2) 1 (+ (fib (- n 1)) (fib (- n 2)))))))
 (print (each fib (range<- 10))))
