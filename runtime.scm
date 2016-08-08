(make-trait miranda-trait me
  ({.print-on sink} (sink .write me))  
  (message (error "Message not understood" message me)))

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
  )

(make-trait claim-primitive me
  ({.print-on sink}
   (sink .display (if me "#yes" "#no")))
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
  ({.not}         (__bit-not me))
  ({.and b}       (__bit-and me b))
  ({.or b}        (__bit-or  me b))
  ({.xor b}       (__bit-xor me b))
  )

(make-trait symbol-primitive me
  ((actor @arguments)
   (call actor (term<- me arguments)))
  ({.name}        (__symbol->string me))
  )

(make-trait nil-primitive me
  ({.empty?}      #yes)
  ({.first}       (error "Empty list" '.first))
  ({.rest}        (error "Empty list" '.rest))
  ({.count}       0)
  ((i)            (error "Empty list" 'nth))
  ({.chain a}     a)
  ({.print-on sink} (sink .display "()"))
  (message        (list-trait me message))) ;XXX use trait syntax instead

(make-trait cons-primitive me
  ({.empty?}      #no)
  ({.first}       (__car me))
  ({.rest}        (__cdr me))
  ({.count}       (__length me))
  ((i)            (__list-ref me i))
  ({.chain a}     (__append me a))
  ({.print-on sink}
   (sink .display "(")
   (sink .print me.first)
   (begin printing ((r me.rest))
     (case ((cons? r)
            (sink .display " ")
            (sink .print r.first)
            (printing r.rest))
           ((null? r))
           (else
            (sink .display " . ")       ;N.B. we're not supporting this in read, iirc
            (sink .print r))))
   (sink .display ")"))
  (message        (list-trait me message))) ;XXX use trait syntax instead

(make-trait vector-primitive me
  ({.empty?}      (= 0 me.count))
  ({.first}       (me 0))
  ({.rest}        (me .slice 1))
  ({.count}       (__vector-length me))
  ((i)            (__vector-ref me i))
  ({.maps? i}     (__vector-maps? me i))
  ({.chain b}     (__vector-append me b))
  ({.slice i}     (__subvector me i me.count))
  ({.slice i j}   (__subvector me i j))
  ({.set! i val}  (__vector-set! me i val))
  ({.print-on sink}
   (sink .display "#")
   (sink .print (__vector->list me)))
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
  ({.print-on sink}
   (sink .display #\")
   (for each! ((c me))
     (sink .display (match c            ;XXX super slow. We might prefer to use the Gambit built-in.
                      (#\\ "\\\\")
                      (#\" "\\\"")
                      (#\newline "\\n")
                      (#\tab     "\\t")
                      (#\return  "\\r")
                      (_ c))))
   (sink .display #\"))
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
  ({.print-on sink}
   (sink .display "<box ")
   (sink .print me.^)
   (sink .display ">"))
  )

(make-trait sink-primitive me
  ({.display a}   (__display me a))
  ({.write a}     (__write me a))     ;XXX Scheme naming isn't very illuminating here
  ({.print a}     (a .print-on me))
  )

(make-trait term-primitive me
  ({.tag}         (__term-tag me))
  ({.arguments}   (__term-arguments me))
  ({.print-on sink}
   (sink .display "{")
   (sink .print me.tag)
   (for each! ((a me.arguments))
     (sink .display " ")
     (sink .print a))
   (sink .display "}"))
  )


;; Continuations

(make __halt-cont
  ({.empty?}        #yes)
  ({.first}         (error "No more frames" __halt-cont))
  ({.rest}          (error "No more frames" __halt-cont))
  ({.print-on sink} (sink .display "<halt-cont>"))
  (message          (list-trait me message))) ;XXX use trait syntax instead

(make-trait __cont-trait me
  ({.empty?}        #no)
  ({.print-on sink} (sink .display "<cont>")) ;XXX more
  (message          (list-trait me message))) ;XXX use trait syntax instead

(define (__match-clause-cont k pat-r body rest-clauses object script datum message)
  (make {extending __cont-trait}
    ({.rest} k)
    ({.first} `((^ ,body) ,@rest-clauses))))

(define (__ev-trait-cont k r trait clauses)
  (make {extending __cont-trait}
    ({.rest} k)
    ({.first} `(make
                 ,trait                ;XXX not source syntax
                 ^ 
                 ,@clauses))))

(define (__ev-make-cont k stamp-val r clauses)
  (make {extending __cont-trait}
    ({.rest} k)
    ({.first} `(make ^ #no   ; XXX as above
                 ,@clauses))))

(define (__ev-do-rest-cont k r e2)
  (make {extending __cont-trait}
    ({.rest} k)
    ({.first} e2)))

(define (__ev-let-match-cont k r p)
  (make {extending __cont-trait}
    ({.rest} k)
    ({.first} `(<match> ,p))))          ;XXX lously presentation

(define (__ev-let-check-cont k val)
  (make {extending __cont-trait}
    ({.rest} k)
    ({.first} `(<assert-matched-then> ',val))))

(define (__ev-arg-cont k r e2)
  (make {extending __cont-trait}
    ({.rest} k)
    ({.first} `(^ ,e2))))

(define (__ev-call-cont k receiver)
  (make {extending __cont-trait}
    ({.rest} k)
    ({.first} `(call ',receiver ^))))

(define (__ev-rest-args-cont k es r vals)
  (make {extending __cont-trait}
    ({.rest} k)
    ({.first}
     (define (quotify v) `',v)
     `(,@(each quotify (reverse vals)) ^ ,@es))))

(define (__ev-tag-cont k tag)
  (make {extending __cont-trait}
    ({.rest} k)
    ({.first} (term<- tag '^^^))))

(define (__ev-and-pat-cont k r subject p2)
  (make {extending __cont-trait}
    ({.rest} k)
    ({.first} `(<and-match?> ,p2))))

(define (__ev-view-call-cont k r subject p)
  (make {extending __cont-trait}
    ({.rest} k)
    ({.first} `(: _ ^ ,p))))

(define (__ev-view-match-cont k r p)
  (make {extending __cont-trait}
    ({.rest} k)
    ({.first} p)))

(define (__ev-match-rest-cont k r subjects ps)
  (make {extending __cont-trait}
    ({.rest} k)
    ({.first} `(<all-match?> ,@ps))))
