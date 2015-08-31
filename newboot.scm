(make-trait halt-cont-primitive halt
  ({.empty?} #yes)
  ({.first}  (error "No more frames" halt))
  ({.rest}   (error "No more frames" halt))
  )

(make-trait nonempty-cont-primitive cont
  ({.empty?} #no)
  ({.rest}   (__nonempty-cont-parent cont))
  )

;; must be composed with nonempty-cont-primitive:
(make-trait let-cont-primitive cont
  ({.first} `(let ,(__let-cont-var cont) ^)) ;XXX proably obsolete
  )

;;XXX and so on for other continuation types

(make-trait claim-primitive claim
  ({.choose if-yes if-no}
   ((__claim-pick claim if-yes if-no)))
  )

(make-trait int32-primitive int32
  ({.+ b}         (__int32+ int32 b))
  ({.- b}         (__int32- int32 b))
  ({.* b}         (__int32* int32 b))
  ({.quotient b}  (__int32-quotient int32 b))
  ({.remainder b} (__int32-remainder int32 b))
  ({.compare b}   (__int32-compare int32 b))
  ({.<< b}        (__int32<<   int32 b))
  ({.not b}       (__int32-not int32 b))
  ({.and b}       (__int32-and int32 b))
  ({.or b}        (__int32-or  int32 b))
  ({.xor b}       (__int32-xor int32 b))
  )

(make-trait symbol-primitive symbol
  ({.name}        (__symbol-name symbol))
  )

(make-trait nil-primitive nil
  ({.empty?}      #yes)
  ({.first}       (error "Empty list" nil))
  ({.rest}        (error "Empty list" nil))
  ({.count}       0)
  ((i)            (error "Empty list" nil))
  ({.chain b}     b)
  )

(make-trait pair-primitive pair
  ({.empty?}      #no)
  ({.first}       (__pair-first pair))
  ({.rest}        (__pair-rest pair))
  )

(make-trait char-primitive char
  ({.code}        (__char-code char))
  ({.letter?}     (__char-letter? char))
  ({.digit?}      (__char-digit? char))
  ({.whitespace?} (__char-whitespace? char))
  ({.alphanumeric?} (or char.letter? char.digit?))
  )

(make-trait string-primitive string
  ({.empty?}      (= 0 string.count))
  ({.first}       (string 0))
  ({.rest}        (string .slice 1))
  ({.count}       (__string-count string))
  ((i)            (__string-ref string i))
  ({.maps? i}     (__string-maps? string i))
  ({.chain b}     (__string-chain string b))
  ({.slice i}     (__string-slice string i string.count))
  ({.slice i j}   (__string-slice string i j))
  )

(make-trait vector-primitive vector
  ({.empty?}      (= 0 vector.count))
  ({.first}       (vector 0))
  ({.rest}        (vector .slice 1))
  ({.count}       (__vector-count vector))
  ((i)            (__vector-ref vector i))
  ({.maps? i}     (__vector-maps? vector i))
  ({.chain b}     (__vector-chain vector b))
  ({.slice i}     (__vector-slice vector i vector.count))
  ({.slice i j}   (__vector-slice vector i j))
  ({.set! i val}  (__vector-set! vector i val))
  )

(make-trait box-primitive box
  ({.^}           (__box-ref box))
  ({.^= val}      (__box-set! box val))
  )

