(define (starts-with? form tag)
  (and (pair? form) (eq? (car form) tag)))

(define (cue<- symbol)
  ;;XXX define a new type instead
  (string->symbol (string-append "." (symbol->string symbol))))

(define (cue? x)
  (and (symbol? x)
       ;; XXX just gonna assume a 0-length symbol won't come up, here
       (char=? (string-ref (symbol->string x) 0)
               #\.)))

;;TODO: make this something like ('coerce boolean? x)
(define (boolean<- x)                   
  (not (not x)))

(define (assert ok? plaint irritant)
  (if (not ok?)
      (error plaint irritant)))

(define (should= x expected)
  (assert (equal? x expected) "Expected" expected))

(define (identity x) x)

(define (foldl f z xs)
  (if (null? xs)
      z
      (foldl f (f z (car xs)) (cdr xs))))

(define (foldr f z xs)
  (if (null? xs)
      z
      (f (car xs) (foldr f z (cdr xs)))))

(define (all f xs)
  (or (null? xs)
      (and (f (car xs))
           (all f (cdr xs)))))

(define (any f xs)
  (and (not (null? xs))
       (or (f (car xs))
           (any f (cdr xs)))))

(define (flatmap f xs)
  (foldr append '() (map f xs)))

(define (last ls)
  (if (null? (cdr ls))
      (car ls)
      (last (cdr ls))))
(define (butlast ls)
  (remove-nth ls (- (length ls) 1)))
(define (remove-nth ls n)
  (if (= 0 n)
      (cdr ls)
      (cons (car ls) 
            (remove-nth (cdr ls) (- n 1)))))

(define (snarf filename reader)
  (call-with-input-file filename
    (lambda (port)
      (let reading ((form (reader port)))
        (if (eof-object? form)
            '()
            (cons form (reading (reader port))))))))

(define (string-join between strings)
  (if (null? strings)
      ""
      (list->string
       (let ((tween (string->list between)))
         (let appending ((ls strings))
           (let ((head (string->list (car ls))))
             (if (null? (cdr ls))
                 head
                 (append head tween (appending (cdr ls))))))))))

(define (expand-mlambda subject clauses)
  (letrec
      ((expand-clause 
        (lambda (clause else-exp)
          (let ((pattern (car clause))
                (then-exp `(begin . ,(cdr clause)))
                (fail (gensym)))
            `(let ((,fail (lambda () ,else-exp)))
               ,(expand-pattern pattern then-exp `(,fail))))))

       (expand-pattern
        (lambda (pattern then-exp else-exp)
          (let ((test-constant
                 (lambda (constant)
                   `(if (eqv? ,subject ',constant) ,then-exp ,else-exp))))
            (cond ((eqv? pattern '_)
                   then-exp)
                  ((starts-with? pattern 'quote)
                   (test-constant (cadr pattern)))
                  ((symbol? pattern)
                   `(let ((,pattern ,subject)) ,then-exp))
                  ((starts-with? pattern ':)
                   (let ((name (cadr pattern)) (predicate (caddr pattern)))
                     `(if (,predicate ,subject)
                          (let ((,name ,subject)) ,then-exp)
                          ,else-exp)))
                  ((pair? pattern)
                   `(if (pair? ,subject)
                        (mcase (car ,subject)
                          (,(car pattern) (mcase (cdr ,subject)
                                            (,(cdr pattern) ,then-exp)
                                            (_ ,else-exp)))
                          (_ ,else-exp))
                        ,else-exp))
                  (else
                   (test-constant pattern)))))))

    (foldr expand-clause `(%match-error ,subject) clauses)))

(define (%match-error subject)
  (error "Match failure" subject))

;; XXX better name? record? struct? row? tagged tuple? glom? functor? (hah)
(define-structure term tag parts)

(define (term<-list list)
  (make-term (car list) (cdr list)))

(define (term<- tag . parts)
  (make-term tag parts))
