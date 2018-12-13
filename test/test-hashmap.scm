(let a (map<-))
(print a)
(print (a .get 42))
(a .set! 'x "yay")
(print a)
(print (a .get 'x))
(a .set! 'x "boo")
(print a)
(print (a .get 'x))
(print (a .get 'y 'nope))

(a .set! 'z "zeee")
(print a)
(print (a .get 'x))
(print (a .get 'y))
(print (a .get 'z))
(print (list<- a.keys a.values a.items a.empty? a.count))
(print (a 'z))

;; TODO more tests

(import (use "lib/random") rng)

(to (random-tests n-trials)            ;TODO use squickcheck
  (for each! ((_ (range<- n-trials)))
    (exercise-em (for each ((value (range<- 50)))
                   (let key (rng .random-integer 16))
                   (let op (match (rng .random-integer 10)
                             (0 'delete)
                             (1 'fetch)
                             (2 'fetch)
                             (3 'fetch)
                             (_ value)))
                   `(,key ,op)))))

(to (exercise-em pairs)
  (let m (map<-))     ;; The hashmap under test.
  (let a (box<- '())) ;; An a-list that should be equivalent.
  (for each! ((`(,key ,op) pairs))
    (match op
      ('fetch
       (let m-val (m .get key))
       (let a-val (match (assoc key a.^)
                    (#no #no)
                    (`(,k ,v) v)))
       (surely (= m-val a-val))
                                        ;        (print `(,key ,m-val))
       )
      ('delete
       (m .delete! key)
       (a .^= (a-list-remove key a.^)))
      (value
       (m .set! key value)
       (a .^= `((,key ,value) ,@(a-list-remove key a.^)))
       ;; TODO test equivalence here
       )))
  (let r1 (sort m.items))
  (let r2 (sort a.^))
  (surely (= r1 r2) "Final maps diverge" r1 r2))

(to (a-list-remove key a-list)
  (for those ((`(,k ,_) a-list))
    (not= k key)))

(random-tests 18)
