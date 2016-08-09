;; SICP exercise 2.29
;; (I renamed 'total-weight' to just 'weight'.)

;; OO version

(define (mobile<- left right)
  (make
    ({.weight}    (+ left.weight right.weight))
    ({.balanced?} (and left.balanced?
                       right.balanced?
                       (= left.torque right.torque)))))

(define (branch<- length submobile)
  (let structure (if (number? submobile) (weight<- submobile) submobile))
  (make
    ({.torque}    (* length structure.weight))
    (msg          (call structure msg))))

(define (weight<- value)
  (make
    ({.weight}    value)
    ({.balanced?} #yes)))

(let test-mobile
  (mobile<- (branch<- 1 10)
            (branch<- 2 (mobile<- (branch<- 3 20)
                                  (branch<- 4 30)))))

(let test-balanced-mobile
  (mobile<- (branch<- 7 10)
            (branch<- 1 (mobile<- (branch<- 3 40)
                                  (branch<- 4 30)))))

(print test-mobile.balanced?)
(print test-balanced-mobile.balanced?)
