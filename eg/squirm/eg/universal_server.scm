;; https://joearms.github.io/published/2013-11-21-My-favorite-erlang-program.html

(to (main)
  (print (test)))

(to (universal-server)
  (? (['become f]
      (f))))

(to (factorial-server)
  (? ([from n]
      (! from (factorial n))
      (factorial-server))))

(to (factorial n)
  (if (= n 0) 1 (* n (factorial (- n 1)))))

(to (test)
  (let pid (spawn universal-server))
  (! pid ['become factorial-server])
  (! pid [(me) 10])
  (? (x x)))
