;; growable mutable vectors (have a better name?)
;; TODO: shrink capacity sometimes

(let fillvector<-
  (make
    (.run arguments
      (fillvector<-vector (call '.run vector<- arguments)))))

(define (fillvector<-count start-count start-value)
  (fillvector<-vector (vector<-count start-count start-value)))

(define (fillvector<-vector start-vector)   ;; (private)
  (let count (box<- (.count start-vector)))
  (let vec   (box<- start-vector))

  (define (grow)
    (let old (vec))
    (let n (.count old))
    (.set! vec (vector<-count (if (= 0 n) 1 (* 2 n))))
    (.copy! vec old))

  (define (count-check i)
    (unless (< i (count))
      (error "Bad index" fillvector i)))

  (make fillvector
    (.count ()
      (count))
    (.run (i)
      (count-check i)
      ((vec) i))
    (.set! (i value)
      (count-check i)
      (.set! (vec) i value))
    (.push! (value)
      (let i (count))
      (if (= i (.count (vec))) (grow) 'pass)
      (.set! (vec) i value)
      (.set! count (+ i 1))
      i)                              ; (should we be returning this?)
    (.pop! ()
      (let i (- (count) 1))
      (when (< i 0)
        (error "Underflow" fillvector))
      (.set! count i)
      ((vec) i))
    (.snapshot ()
      (.slice (vec) 0 (count)))         ;XXX make immutable
    (.copy! (v lo hib)
      (count-check hib)
      (.copy! (vec) lo hib))

    ;; XXX should be vector trait...
    (.empty? ()      (= 0 (.count fillvector)))
    (.first ()       (fillvector 0))
    (.rest ()        (.slice fillvector 1))
    (.copy! (v)      (.copy! fillvector v 0 (.count v)))
    (.slice (lo)     (.slice fillvector lo (.count fillvector)))
    ;; inefficient:
    (.chain (v)      (.chain (.snapshot fillvector) v))
    (.slice (lo hib) (.slice (.snapshot fillvector) lo hib))
    ))
