(define (cons car cdr)
  (make pair
    (.first () car)
    (.rest () cdr)))


;; Another cut:

(let list-trait
  (make-trait list
    ((i)
     (if (= 0 i)
         (list .first)
         ((list .rest) (- i 1))))
    ({.count}
     (if (list .empty?)
         0
         (+ 1 ((list .rest) .count))))
    ...))

(let (pair? pair-stamp) (stamp<- 'pair))

(define (cons car cdr)
  (make pair pair-stamp list-trait
    ({.empty?} #no)
    ({.first} car)
    ({.rest} cdr)))


;; And maybe:

(let list-trait
  (make-trait list
    ((i)
     (if (= 0 i)
         list.first
         (list.rest (- i 1))))
    ({.count}
     (if list.empty?
         0
         (+ 1 list.rest.count)))
    ...))

;; XXX but that's ambiguous!
;; Does (a.b x) mean ((a .b) x) or (a .b x)?
;; So resolve this with {}?
;; Like, {.d x} conses data;
;;       {o.m x} sends o {.m x};
;;       (o.m x) sends o {.m}, then (x) to the result.
;; Well...
;;       o.m{x}?
;; How to write ((f x) .m)?
;; Just like that? Because (f x).m sure won't fly.
;;
;; For a start, ... o .m ... needs to mean the same as ... o.m ...
;; or else nested expressions will pile up without any whitespace.
;; Therefore, (list.empty?) should mean (list .empty?)
;; and then I'm not seeing any real point to the space-removal.

;; Resolving, then:
;; a.b.c ==> ((a .b) .c) meaning a gets {.b}, result gets {.c}
;; (a .d x) means a gets {.d x}
;; {.d x} conses data
;; (f x y) means (f .list x y) meaning f gets {.list x y} -- or .tuple?


;; In T:

(define-predicate pair?)
(define-settable-operation (car pair))
(define-settable-operation (cdr pair))

(define (cons the-car the-cdr)
  (object nil
    ((pair? self) t)
    ((car self) the-car)
    ((cdr self) the-cdr)
    (((setter car) self new-car) (set the-car new-car))
    (((setter cdr) self new-cdr) (set the-cdr new-cdr))))
