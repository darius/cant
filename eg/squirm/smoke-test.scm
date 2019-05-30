(to (main)
  (let pid (spawn (given ()
                    (report "hey")
                    (report "dud")
                    (? (msg (report (cons 'got (cons msg '())))))
                    (report "dude")
                    (report (fact 15)))))
  (report (cons 'pid (cons pid '())))
  (! pid 'yoohoo)
  (report (fact 10)))

(to (report x)
  (print (cons (me) (cons x '()))))

(to (fact n)
  (if (= n 0)
      1
      (do (let x (fact (- n 1)))
          (* n x))))
