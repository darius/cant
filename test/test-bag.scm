;; TODO this is a dupe of test-hashmap, c'mon

(let a (bag<-))
(print a)
(print (a .get 42))
(a .add! 'x)
(print a)
(print (a .get 'x))
(a .add! 'x)
(print a)
(print (a .get 'x))
(print (a .get 'y 'nope))

(a .add! 'z)
(print a)
(print (a .get 'x))
(print (a .get 'y))
(print (a .get 'z))
(print (list<- a.keys a.values a.items a.empty? a.count))
(print (a 'z))

;; TODO more tests

(import (use "lib/random") rng<-)

(let rng (rng<- 1234567))

(to (random-tests n-trials)            ;TODO use squickcheck
  (for each! ((_ (range<- n-trials)))
    (exercise-em (for each ((value (range<- 50)))
                   (rng .random-integer 5)))))

(to (exercise-em keys)
  (let m (bag<-))     ;; The bag under test.
  (let a (box<- '())) ;; A list of keys seen so far.
  (for each! ((key keys))
    (let m-val (m .get key 0))
    (let a-val ((for those ((k a.^)) (= k key)) .count))
    (surely (= m-val a-val) "mismatch" key m-val a-val)

    (m .add! key)
    (a .^= (cons key a.^))))

(random-tests 13)
