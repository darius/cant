(make-trait number-primitive me
  ({.+ a} (__+ me a))
  ({.- a} (__- me a))
  ({.* a} (__* me a))
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

