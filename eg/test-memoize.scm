(hide
 (let fib (memoize
           (given (n)
             (if (< n 2) 1 (+ (fib (- n 1)) (fib (- n 2)))))))
 (print (each fib (range<- 10)))
)
