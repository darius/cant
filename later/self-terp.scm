(define (constant<- value)
  (make
    (.evaluate (r) value)))

(define (variable<- name)
  (make
    (.evaluate (r) (r name))))

(define (make<- script)
  (make
    (.evaluate (r) (object<- script r)))) ;XXX needs expansion

(define (letrec<- defns body)
  (make
    (.evaluate (r)
      (let new-r (env-extend-promises r (map first/0 defns)))
      (for each! ((defn defns))
        (env-resolve! new-r
                      (defn 0)
                      (.evaluate (defn 1) new-r)))
      (evaluate body new-r))))

(define evaluate/1 (selector<- 'evaluate 1))
(define first/0    (selector<- 'first 0))

(define (call<- cue addressee operands)
  (let selector (selector<- cue (.count operands)))
  (make
    (.evaluate (r)
      (call selector (.evaluate addressee r) (map evaluate/1 operands))))))
