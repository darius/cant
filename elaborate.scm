;; Expanding out syntactic sugar

(define (elaborate e)
  (cond ((symbol? e) e)
        ((self-evaluating? e) (list 'quote e))
        (else
         (assert (pair? e) "Bad syntax" e)
         (case (car e)
           ((quote)
            e)
           ((begin)
            (elaborate-seq (cdr e)))
           ((make)
            (let ((make<-
                   (lambda (methods)
                     `(make ,@(map elaborate-method/matcher methods)))))
              (if (and (not (null? (cdr e)))
                       (symbol? (cadr e)))
                  (elaborate `(letrec ((,(cadr e) ,(make<- (cddr e))))
                                ,(cadr e)))
                  (make<- (cdr e)))))
           ((letrec)
            (if (null? (cadr e))
                (elaborate-seq (cddr e))
                `(letrec ,(map (lambda (defn)
                                 (list (car defn) (elaborate (cadr defn))))
                               (cadr e))
                   ,(elaborate-seq (cddr e)))))
           ((let)
            (elaborate `('run (make ('run ,(map car (cadr e))
                                      ,@(cddr e)))
                              ,@(map cadr (cadr e)))))
           ((lambda)
            (elaborate `(make ('run ,(cadr e) ,@(cddr e)))))
           ((if)
            (let ((test (cadr e)) (if-true (caddr e)) (if-false (cadddr e)))
              ;; XXX I suspect the boolean coercion is a bad idea in
              ;; our context: there's too much polymorphism.
              (elaborate `('choose ('run ',boolean<- ,test)
                                   (lambda () ,if-true)
                                   (lambda () ,if-false)))))
           (else
            (if (and (starts-with? (car e) 'quote)
                     (symbol? (cadar e)))
                (cons (car e) (map elaborate (cdr e)))
                (cons ''run (map elaborate e))))))))  ; default cue

(define (elaborate-method/matcher method)
  (assert (or (eq? (car method) 'else)
              (starts-with? (car method) 'quote))
          "Bad method/matcher syntax" method)
  `(,(car method) ,(cadr method)
    ,(elaborate-seq (cddr method))))

(define (elaborate-seq es)
  (begin<- (map elaborate es)))

(define (begin<- es)
  (cond ((null? es) ''#f)
        ((null? (cdr es)) (car es))
        (else (begin2<- (car es) (begin<- (cdr es))))))

(define (begin2<- e1 e2)
  `('run (make ('run (v thunk) ('run thunk)))
         ,e1
         (make ('run () ,e2))))

(define (self-evaluating? x)
  (or (boolean? x)
      (number? x)
      (char? x)
      (string? x)))


;; Smoke test of elaboration

(should= (elaborate '42)
         ''42)
(should= (elaborate '#f)
         ''#f)
(should= (elaborate ''())
         ''())
(should= (elaborate '(begin (write x) (lambda (y) ('+ x y))))
         '('run
           (make ('run (v thunk) ('run thunk)))
           ('run write x)
           (make ('run () (make ('run (y) ('+ x y)))))))
'(should= (elaborate '(begin (if x y z)))
         '('choose x (make ('run () y)) (make ('run () z))))
'(should= (elaborate '(lambda ()
                      (letrec ((for-each
                                (lambda (f xs)
                                  (if (null? xs)
                                      '()
                                      (begin (f ('car xs))
                                             (for-each f ('cdr xs)))))))
                        for-each)))
         '(make
           ('run ()
             (letrec ((for-each
                       (make
                        ('run (f xs)
                          ('choose
                           ('run null? xs)
                           (make ('run () '()))
                           (make
                            ('run ()
                              ('run
                               (make ('run (v thunk) ('run thunk)))
                               ('run f ('car xs))
                               (make
                                ('run () ('run for-each f ('cdr xs))))))))))))
               for-each))))
