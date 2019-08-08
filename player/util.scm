(library (player util)
(export report starts-with? cue<- cue? boolean<- insist should= identity foldl
        foldr all any flatmap last butlast remove-nth snarf string-join
        term<-list term<- make-term term? term-tag term-parts)
(import (chezscheme))

(define (report x)
  (write x)
  (newline))

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

;;TODO Chez has a built-in (assert ok?)
(define (insist ok? plaint irritant)
  (if (not ok?)
      (error 'insist plaint irritant)))

(define (should= x expected)
  (insist (equal? x expected) "Expected" expected))

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

;; XXX better name? record? struct? row? tagged tuple? glom? functor? (hah)
;(define-structure term tag parts)
(define-record-type term (fields tag parts))
  
(define (term<-list list)
  (make-term (car list) (cdr list)))

(define (term<- tag . parts)
  (make-term tag parts))

)
