;; Basic PEG-ish parsing

;; TODO: track farthest reached
;; TODO: 'not' combinator
;; TODO: reasonable efficiency
;; TODO: memoize

(define (succeed chars vals)
  (list<- chars vals))

(define (fail chars vals)
  #f)

(define (folded<- combine)
  (make
    ('run (p) p)
    ('run (p q) (combine p q))
    ('run (p q r) (combine p (combine q r)))
    ('run (p q r s) (combine p (combine q (combine r s))))
    ))

(define alt
  (folded<- (lambda (p q)
              (lambda (chars vals)
                (let ((result (p chars vals)))
                  (if result result (q chars vals)))))))

(define seq
  (folded<- (lambda (p q)
              (lambda (chars vals)
                (let ((result (p chars vals)))
                  (if result
                      (q (result 0) (result 1))
                      #f))))))

(define (feed f)
  (lambda (chars vals)
    (list<- chars (list<- (call 'run f vals)))))

(define (eat1 ok?)
  (lambda (chars vals)
    (if ('empty? chars)
        #f
        (if (ok? ('first chars))
            (list<- ('rest chars) (chain vals (list<- ('first chars))))
            #f))))
    
(define (skip1 ok?)
  (lambda (chars vals)
    (if ('empty? chars)
        #f
        (if (ok? ('first chars))
            (list<- ('rest chars) vals)
            #f))))
    
(define any1 (eat1 (lambda (char) #t)))

(define (lit1 my-char)
  (skip1 (lambda (char) (is? my-char char))))

;; Smoke test

(define bal
  (lambda (cs vs)
    ((alt (seq (lit1 #\() bal (lit1 #\)) bal)
          succeed)
     cs vs)))

(print (bal "(abc" '()))
(print (bal "()xyz" '()))
(print (bal "()()xyz" '()))
(print (bal "(()(()))" '()))
