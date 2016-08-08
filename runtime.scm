(make-trait list-trait list
  ((i)
   (if (= i 0)
       list.first
       (list.rest (- i 1))))
  ({.empty?}
   (= 0 list.count)) ;N.B. these default implementations are circular
  ({.first}
   (list 0))
  ({.rest}
   (list .slice 1))
  ({.count}
   (if list.empty?
       0
       (+ 1 list.rest.count)))
  ({.slice i}
   (assert (<= 0 i))
   (if (= i 0)
       list
       (list.rest .slice (- i 1))))
  ({.slice i bound}     ;XXX result is a cons-list; be more generic?
   (assert (<= 0 i))
   (case (list.empty? list)
         ((<= bound i) '())
         ((= i 0) (cons list.first (list.rest .slice 0 (- bound 1))))
         (else (list.rest .slice (- i 1) (- bound 1)))))
  ({.chain seq}
   (if list.empty?
       seq
       (cons list.first (list.rest .chain seq))))
  ;; A sequence is a kind of collection. Start implementing that:
  ({.maps? key}
   (and (not list.empty?)
        (or (= 0 key)
            (and (< 0 key)
                 (list.rest .maps? (- key 1))))))
  ({.maps-to? value}
   (for some ((x list)) (= x value)))
  ({.find-key-for value}                  ;XXX name?
   (case (list.empty? (error "Missing key" value))
         ((= value list.first) 0)
         (else (+ 1 (list.rest .find-key-for value)))))
  ;;...
  )

(make-trait claim-primitive me
  )

(make-trait procedure-primitive me
  )

(make-trait number-primitive me
  ({.+ a} (__+ me a))
  ({.- a} (__- me a))
  ({.* a} (__* me a))
  ({.quotient b}  (__quotient me b))
  ({.remainder b} (__remainder me b))
;  ({.compare b}   (__number-compare me b))
  ({.<< b}        (__bit-<<  me b))
  ({.not b}       (__bit-not me b))
  ({.and b}       (__bit-and me b))
  ({.or b}        (__bit-or  me b))
  ({.xor b}       (__bit-xor me b))
  )

(make-trait symbol-primitive me
  ((actor @arguments)
   (call actor (term<- me arguments)))
  ({.name}        (__symbol->string me))
  )

(make-trait list-primitive me
  ({.empty?}      (null? me))
  ({.first}       (__car me))
  ({.rest}        (__cdr me))
  ({.count}       (__length me))
  ((i)            (__list-ref me i))
  ({.chain a}     (__append me a))
  ({.maps-to? value}
   (for some ((x me)) (= x value)))     ;XXX 'some' from stdlib
  ({.find-key-for value}                  ;XXX name?
   (case (me.empty? (error "Missing key" value))
         ((= value me.first) 0)
         (else (+ 1 (me.rest .find-key-for value)))))
  )

(make-trait vector-primitive me
  ({.empty?}      (= 0 me.count))
  ({.first}       (me 0))
  ({.rest}        (me .slice 1))
  ({.count}       (__vector-count me))
  ((i)            (__vector-ref me i))
  ({.maps? i}     (__vector-maps? me i))
  ({.chain b}     (__vector-append me b))
  ({.slice i}     (__subvector me i me.count))
  ({.slice i j}   (__subvector me i j))
  ({.set! i val}  (__vector-set! me i val))
  )

(make-trait string-primitive me
  ({.empty?}      (= 0 me.count))
  ({.first}       (me 0))
  ({.rest}        (me .slice 1))
  ({.count}       (__string-length me))
  ((i)            (__string-ref me i))
  ({.maps? i}     (__string-maps? me i))
  ({.chain b}     (__string-append me b))
  ({.slice i}     (__substring me i me.count))
  ({.slice i j}   (__substring me i j))
  )

(make-trait char-primitive me
  ({.code}        (__char->integer me))
  ({.letter?}     (__char-letter? me))
  ({.digit?}      (__char-digit? me))
  ({.whitespace?} (__char-whitespace? me))
  ({.alphanumeric?} (or me.letter? me.digit?))
  )

(make-trait box-primitive me
  ({.^}           (__box-value me))
  ({.^= val}      (__box-value-set! me val))
  )

