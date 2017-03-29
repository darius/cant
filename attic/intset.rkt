#lang racket

;; An example from William Cook's essay on OOP vs. ADTs
;; http://www.cs.utexas.edu/~wcook/Drafts/2009/essay.pdf

(define empty
  (new (class object%
         (super-new)
         (define/public (empty?)   true)
         (define/public (has? k)   false)
         (define/public (adjoin k) (adjoin<- k empty))
         (define/public (merge s)  s))))

(define (adjoin<- n s)
  (if (send s has? n)
      s
      (make extension% [n n] [s s])))

(define extension%
  (class object%
    (init n s)
    (define my-n n)
    (define my-s s)
    (super-new)
    (define/public (empty?)   false)
    (define/public (has? k)   (or (= my-n k) (send my-s has? k)))
    (define/public (adjoin k) (adjoin<- k this))
    (define/public (merge s)  (new merge% [s1 this] [s2 s]))))

(define merge%
  (class object%
    (init s1 s2)
    (define my-s1 s1)
    (define my-s2 s2)
    (super-new)
    (define/public (empty?)   (and (send my-s1 empty?) (send my-s2 empty?)))
    (define/public (has? k)   (or (send my-s1 has? k) (send my-s2 has? k)))
    (define/public (adjoin k) (adjoin<- k this))
    (define/public (merge s)  (new merge% [s1 this] [s2 s]))))

;; Smoke test

(let [(eg (send (send empty adjoin 6) adjoin 5))]
  (print (send eg has? 5))
  (print (send eg has? 6))
  (print (send eg has? 7)))
