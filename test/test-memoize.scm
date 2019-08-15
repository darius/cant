(import (use 'memoize) memoize)

(let fib (memoize
          (on (n)
            (if (< n 2) 1 (+ (fib (- n 1)) (fib (- n 2)))))))
(print (each fib (0 .to< 10)))
