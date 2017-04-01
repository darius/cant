;; A (very slightly) Smalltalk-72-like open interpreter.
;; I think the key thing still to do is this:
;; In an expression like (1 + (t length)) I have to parenthesize
;; the (t length), where Smalltalk-72 would have (1 + t length).
;; Here's how I think that's supposed to work:
;; The send-number procedure needs to call terp with the whole
;; (t length) remainder. (Same change for the $ primitive.)
;; I may be confused about the rest here:
;; The object bound to t looks for a message in "... length",
;; and eats it if it finds it, but if it doesn't find a message
;; it understands, it leaves the tail alone and returns itself.
;; So:
;; 1. Don't raise an error if you see no message you understand.
;;    (Expect debugging troubles from this.)
;; 2. Make the message frames mutable?
;;    *Probably* easier to implement that way.


;; Example programs in the language:

(to (main _)
  (print (a0))
  (print (a1))
  (print (a2))

  (print (b0))
  (print (b1))

  (print (c0))
  (print (c1))

  (print (d0))
  (print (d1))
  (print (d2))

  (print (t4))

  (print (e0))
  (print (e1))
  (print (e2))
)


(to (a0) (run '(#yes >> 42 137)))
(to (a1) (run '(#no >> 42 137)))
(to (a2) (run '(2 + 3)))

(to (b0) (run '((: $) 42)))
(to (b1) (run '((: ($)) 42)))

(to (c0) (run '((: 5 + $) 67)))
(to (c1) (run '((: $ + $) 5 67)))

(to (d0) (run '((: % length) length)))
(to (d1) (run '((: % length >> 1 % not-length >> 2) not-length)))
(to (d2) (run '((: % length >> 0) length)))

(to (t4) (run '((: $ length) (: % length >> 42))))

(let eg-empty
  '(
    let empty (: % length >> 10)
    empty length + 1
    ))
(let eg-pair-0
  '(
    let empty (: % length >> 0)
    let pair
       (: let t $
          : % length >> (1 + (t length)))
    (pair empty) length
    ))
;; Here's the only 'real' example:
;; Define constructors equivalent to Lisp's NIL and CONS,
;; plus CAR, CDR, LENGTH methods. (Actually HD and TL.)
(let eg-pair-1
  '(
    let empty (: % length >> 10)
    let pair
       (: let h $
          let t $
          : % hd     >> h
            % tl     >> t
            % length >> (t length + 1))
    (pair 1 (pair 2 empty)) length
    ))

(to (e0) (run eg-empty))
(to (e1) (run eg-pair-0))
(to (e2) (run eg-pair-1))


;; OK, now to implement it.

(to (run e)
  (terp e '() '(halt)))

(to (trace x) 'ok)

(to (terp e r k)
  (trace `(terp ,e ,r ,k))
  (match e
    ('$                             (prim-$ k))
    ((: symbol?)                    (return k (lookup r e)))
    (`(: ,@rest)                    (return k (make-definition rest r)))
    (`(% ,pattern ,@rest)           (terp-% pattern (nest rest r k)))
    (`(let ,variable ,val-e ,@rest) (terp val-e r `(let ,variable ,rest ,r ,k)))
    (`(,first ,@rest)               (terp first r (nest rest r k)))
    ('()                            (error "empty expression"))
    (_                              (return k e))))

(to (nest es r k)
  (if es.empty? k `(nest ,es ,r ,k)))

(to (make-message e r k)
  ;; XXX null message probably should be kept instead
  ;; e.g. so you'd get an error on too many $'s
  (if (null? e) k `(message ,e ,r ,k)))

(to (return k value)
  (match k
    (`(nest ,@_)
     (send value k))
    (`(message ,@_) ;XXX I don't know what I'm doing. This clause wasn't in the Scheme version.
     (send value k))
    (`(let ,variable ,rest ,r2 ,k2)
     (terp rest `((,variable ,value) ,@r2) k2))
    (`(number-+ ,self ,k2)
     (return k2 (+ self value)))
    ('(halt)
     value)
    (_ (error "Unknown continuation type" k))))

(to (send object kk)
  (let k (to-message kk))
  (trace `(send ,object ,k))
  (match object
    ((: number?) (send-number object k))
    ((: claim?)  (send-claim object k))
    (`(,tag ,e ,r)
     (surely (= tag tag-definition))
     (terp e r k))
    (_ (error "Unknown object type" object))))

(to (to-message k)
  (match k
    (`(nest ,@rest) `(message ,@rest))
    (`(message ,@_) k)                   ;I guess?
    (_ (error "Unexpected cont type" k))))

(to (send-number self k)
  (match k
    (`(message ,e2 ,r2 ,k2)
     (match e2
       (`(+ ,e3 ,@rest)
        (terp e3 r2
              `(number-+ ,self ,(nest rest r2 k2))))))))

(to (send-claim self k)
  (match k
    (`(message ,e2 ,r2 ,k2)
     (match e2
       (`(>> ,then ,@rest) (terp (if self then rest) r2 k2))))))

  

(to (prim-$ k)
  (extract-message k (given (replace `(,e ,@es) r2 k2)
                       (terp e r2
                             (replace (make-message es r2 k2))))))

(to (terp-% pattern k)
  (trace `(terp-% (pattern: ,pattern) (k: ,k)))
  (extract-message k (given (replace e2 r2 k2)
                       (if (= pattern e2.first)
                           (return (replace (make-message e2.rest r2 k2)) #yes)
                           (return k #no)))))

(to (extract-message k take-message)
  (begin walking ((k k) (replace identity))
    (match k
      (`(message ,e2 ,r2 ,k2)
       (take-message replace e2 r2 k2))
      (_ (walking k.last (given (k-prime)
                           (chain (but-last k) `(,k-prime))))))))

(to (but-last xs)
  (match xs
    (`(,_) '())
    (`(,x ,@rest) `(,x ,@(but-last rest)))))

;; N.B. that'd be easier if we kept the k in the *first* slot


(to (make-definition e r)
  `(,tag-definition ,e ,r))

(make tag-definition)

(to (lookup r name)
  (let `(,_ ,value) (assq name r))
  value)

(export main)
