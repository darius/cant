(newline)

(hide
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

 (define (random-tests n-trials)
   (for each! ((_ (range<- n-trials)))
        (exercise-em (for each ((value (range<- 50)))
                          (let key (random-integer 16))
                          (let op  (if (< (random-integer 10) 3)
                                       'fetch
                                       value))
                          `(,key ,op)))))

 (define (exercise-em pairs)
   (let m (map<-))     ;; The hashmap under test.
   (let a (box<- '())) ;; An a-list that should be equivalent.
   (for each! (((key op) pairs))
        (match op
               ('fetch
                (let m-val (m .get key))
                (let a-val (match (assoc key a.^)
                                  (#no #no)
                                  ((k v) v)))
                (assert (= m-val a-val))
                (print `(,key ,m-val))
                )
               (value
                (m .set! key value)
                (a .^= `((,key ,value) ,@a.^))
                ;; TODO test equivalence here
                ))))

 ;;(random-tests 400)
)
