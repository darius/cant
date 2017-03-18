;; SICP exercise 2.29
;; (I renamed 'total-weight' to just 'weight'.)

;; OO version

(to (mobile<- left right)
  (make _
    ({.weight}    (+ left.weight right.weight))
    ({.balanced?} (and left.balanced?
                       right.balanced?
                       (= left.torque right.torque)))))

(to (branch<- length submobile)
  (let structure (if (number? submobile) (weight<- submobile) submobile))
  (make _
    ({.torque}    (* length structure.weight))
    (msg          (call structure msg))))

(to (weight<- value)
  (make _
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
