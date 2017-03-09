;; This special source file must be written to load without actually
;; invoking any primitive object, because this code *defines* their
;; scripts. Only after this file is loaded will the definitions get
;; magically connected to the primitives.

;; That works out because the top level here is just definitions, with
;; no top-level actions.

(make-trait miranda-trait me
  ({.selfie sink} (sink .write me))  
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
       (+ 1 list.rest.count)))          ;TODO tail recursion
  ({.slice i}
   (surely (<= 0 i))
   (if (= i 0)
       list
       (list.rest .slice (- i 1))))
  ({.slice i bound}     ;XXX result is a cons-list; be more generic?
   (surely (<= 0 i))
   (case (list.empty? list)
         ((<= bound i) '())
         ((= i 0) (cons list.first (list.rest .slice 0 (- bound 1))))
         (else (list.rest .slice (- i 1) (- bound 1)))))
  ({.chain seq}
   (if list.empty?
       seq
       (cons list.first (list.rest .chain seq))))
  ({.compare xs}
   ;; N.B. mutable vectors compare by this method, so it's really a comparison as of right now
   (case (list.empty? (if xs.empty? 0 -1))
         (xs.empty? 1)
         (else (match (list.first .compare xs.first)
                 (0 (list.rest .compare xs.rest))
                 (d d)))))
  ;; A sequence is a kind of collection. Start implementing that:
  ({.keys}
   (range<- list.count))
  ({.values}
   list)
  ({.items}
   (enumerate list))
  ({.get key}
   (list .get key #no))
  ({.get key default}
   (if (and (integer? key) (<= 0 key) (not list.empty?))
       (begin walking ((k key) (xs rest))
         (case ((= k 0) xs.first)
               (xs.empty? default)
               (else (walking (- k 1) xs.rest))))
       default))
  ({.maps? key}
   (and (not list.empty?)
        (or (= 0 key)
            (and (< 0 key)
                 (list.rest .maps? (- key 1))))))
  ({.find value default}    ;; XXX update the other collections to have this too
   (begin looking ((i 0) (values list))
      (case (values.empty? default)
            ((= value values.first) i)
            (else (looking (+ i 1) values.rest)))))   
  ({.find value}
   (match (list .find value #no)
     (#no (error "Missing value" value))
     (key key)))
  ({.find? value}
   (match (list .find value #no)
     (#no #no)
     (_ #yes)))
  ({.last}
   (let rest list.rest)
   (if rest.empty? list.first rest.last))
  )

(make-trait claim-primitive me
  ({.selfie sink}
   (sink .display (if me "#yes" "#no")))
  ({.compare a}
   (and (claim? a)
        (case ((= me a) 0)
              (me       1)
              (_       -1))))
  )

(make-trait procedure-primitive me
  )

(make-trait number-primitive me
  ({.+ a}         (__+ me a))
  ({.- a}         (__- me a))
  ({.* a}         (__* me a))
  ({.compare a}   (__number-compare me a))
  ({.quotient b}  (__quotient me b))
  ({.remainder b} (__remainder me b))
  ({.modulo b}    (__modulo me b))
  ({.<< b}        (__bit-<<  me b))
  ({.>> b}        (__bit->>  me b))
  ({.not}         (__bit-not me))
  ({.and b}       (__bit-and me b))
  ({.or b}        (__bit-or  me b))
  ({.xor b}       (__bit-xor me b))
  ;; XXX sketchy support for 32-bit word ops:
  ({.u+ a}        (__u+ me a))
  ({.u/ a}        (__u/ me a))
  ({.u>> a}       (__u>> me a))
  ({.u<< a}       (__u<< me a))
  ({.s+ a}        (__s+ me a))
  ({.s* a}        (__s* me a))
  )

(make-trait symbol-primitive me
  ((actor @arguments)
   (call actor (term<- me arguments)))
  ({.name}        (__symbol->string me))
  ({.compare a}   (and (symbol? a)
                       (me.name .compare a.name)))
  ({.selfie sink} (sink .display me.name))
  )

(make-trait nil-primitive me
  ({.empty?}      #yes)
  ({.first}       (error "Empty list" '.first))
  ({.rest}        (error "Empty list" '.rest))
  ({.count}       0)
  ((i)            (error "Empty list" 'nth i))
  ({.chain a}     a)
  ({.selfie sink} (sink .display "()"))
  (message        (list-trait me message))) ;XXX use trait syntax instead

(make-trait cons-primitive me
  ({.empty?}      #no)
  ({.first}       (__car me))
  ({.rest}        (__cdr me))
  ({.count}       (__length me))
  ((i)            (__list-ref me i))    ;XXX just use the trait method? then can e.g. mix lazy and eager list nodes
  ({.chain a}     (__append me a))
  ({.selfie sink}
   (sink .display "(")
   (sink .print me.first)
   (begin printing ((r me.rest))
     (case ((cons? r)
            (sink .display " ")
            (sink .print r.first)
            (printing r.rest))
           ((null? r))
           (else
            (sink .display " . ")       ;XXX we're not supporting this in read, iirc
            (sink .print r))))
   (sink .display ")"))
  (message
   (list-trait me message))) ;XXX use trait syntax instead

(make-trait vector-primitive me
  ({.empty?}      (= 0 me.count))
  ({.first}       (me 0))
  ({.rest}        (me .slice 1))
  ({.count}       (__vector-length me))
  ((i)            (__vector-ref me i))
  ({.maps? i}     (__vector-maps? me i))
  ({.chain v}     (__vector-append me v))
  ({.slice i}     (__subvector me i me.count))
  ({.slice i j}   (__subvector me i j))
  ({.set! i val}  (__vector-set! me i val))
  ({.copy! v}     (me .copy! v 0 v.count))
  ({.copy! v lo bound}
   ;; XXX range-check first
   (for each! ((i (range<- lo bound)))
     (__vector-set! me (- i lo) (v i)))) ;XXX was this what I wanted? I forget.
  ({.copy}        (__vector-copy me))
  ({.move! dest src len}                ;XXX untested
   (for each! ((i (if (<= dest src)
                      (range<- len)
                      (reverse (range<- len)))))  ;TODO inefficient
     (me .set! (+ dest i)
         (me (+ src i)))))
  ({.last}
   (me (- me.count 1)))
  ({.selfie sink}
   (sink .display "#!")
   (sink .print (__vector->list me)))
  (message
   (list-trait me message))) ;XXX use trait syntax instead

(make-trait string-primitive me
  ({.empty?}      (= 0 me.count))
  ({.first}       (me 0))
  ({.rest}        (me .slice 1))
  ({.count}       (__string-length me))
  ((i)            (__string-ref me i))
  ({.maps? i}     (__string-maps? me i))
  ({.chain s}     (__string-append me s))
  ({.slice i}     (__substring me i me.count))
  ({.slice i j}   (__substring me i j))
  ({.compare s}
   (if (string? s)
       (__string-compare me s)          ; just a speedup
       (list-trait me {.compare s})))   ; but is this what we really want? (<=> "a" '(#\a))
  ({.join ss}   ;should this be a function, not a method?
   (if ss.empty?
       ""
       (foldr1 (given (x y) (chain x me y)) ss)))
  ;;XXX below mostly from list-trait, until .selfie
  ({.keys}        (range<- me.count))
  ({.values}      me)
  ({.items}       (enumerate me))
  ({.get key}     (me .get key #no))
  ({.get key default}
   (if (me .maps? key)
       (me key)
       default))
  ({.maps? key}                         ;XXX duplicate, see above
   (and (integer? key) (<= 0 key) (< key me.count)))
  ({.trim-left}
   (if me.empty?
       me
       (do (let c me.first)
           (if c.whitespace?
               me.rest.trim-left
               me))))
  ({.trim-right}
   (begin scanning ((i me.count))
     (if (= i 0)
         ""
         (do (let c (me (- i 1)))
             (if c.whitespace?
                 (scanning (- i 1))
                 (me .slice 0 i))))))
  ({.trim}
   me.trim-left.trim-right)
  ({.split}
   (begin splitting ((s me.trim-left))
     (if s.empty?
         '()
         (do (let limit s.count)
             (begin scanning ((i 1))
               (case ((= i limit) `(,s))
                     (((s i) .whitespace?)
                      (cons (s .slice 0 i)
                            (splitting ((s .slice (+ i 1)) .trim-left))))
                     (else (scanning (+ i 1)))))))))
  ({.split delimiter}
   ;; TODO deduplicate code
   (begin splitting ((s me))
     (if s.empty?
         '()
         (do (let limit s.count)
             (begin scanning ((i 0))
               (case ((= i limit) `(,s))
                     ((= delimiter (s .slice i (+ i delimiter.count)))
                      (cons (s .slice 0 i)
                            (splitting (s .slice (+ i delimiter.count)))))
                     (else (scanning (+ i 1)))))))))
  ({.lowercase} (string<-list (for each ((c me)) c.lowercase)))
  ({.uppercase} (string<-list (for each ((c me)) c.uppercase)))
  ({.starts-with? s}
   (= (me .slice 0 s.count) s))   ;TODO more efficient
  ({.replace pattern replacement} ;TODO more efficient
   ;; TODO unify the cases?
   (case (pattern.empty?
          (for foldr ((ch me) (rest replacement))
            (chain replacement (string<- ch) rest)))
         (else
          (let limit me.count)
          (string<-list
           (begin scanning ((i 0))
             (case ((= i limit) '())
                   ((= pattern (me .slice i (+ i pattern.count)))
                    (chain (list<-string replacement)
                           (scanning (+ i pattern.count))))
                   (else (cons (me i) (scanning (+ i 1))))))))))
  ({.center n}
   (let pad (- n me.count))
   (if (<= pad 0)
       me
       (do (let half (pad .quotient 2))
           (chain (" " .repeat (- pad half))
                  me
                  (" " .repeat half)))))
  ({.repeat n}
   ;; XXX what if n=0
   (call chain (for each ((_ (range<- n)))
                 me)))
  ({.format @arguments}
   (let sink (string-sink<-))
   (call format `{.to ,sink ,me ,@arguments})
   sink.output-string)
  ({.split-lines}
   (me .split "\n"))
  ({.selfie sink}
   (sink .display #\")
   (for each! ((c me))
     (sink .display (match c            ;XXX super slow. We might prefer to use the Gambit built-in.
                      (#\\ "\\\\")
                      (#\" "\\\"")
                      (#\newline "\\n")
                      (#\tab     "\\t")
                      (#\return  "\\r")
                      ;; XXX escape the control chars
                      (_ c))))
   (sink .display #\"))
  (message
   (list-trait me message))) ;XXX use trait syntax instead

(make-trait char-primitive me
  ({.code}        (__char->integer me))
  ({.letter?}     (__char-letter? me))
  ({.digit?}      (__char-digit? me))
  ({.whitespace?} (__char-whitespace? me))
  ({.alphanumeric?} (or me.letter? me.digit?))
  ({.lowercase}   (__char-lowercase me))
  ({.uppercase}   (__char-uppercase me))
  ({.compare c}   (__char-compare me c)) ;XXX untested
  )

(make-trait box-primitive me
  ({.^}           (__box-value me))
  ({.^= val}      (__box-value-set! me val))
  ({.selfie sink}
   (sink .display "<box ")
   (sink .print me.^)
   (sink .display ">"))
  )

(make-trait source-primitive me
  ({.read-char}   (__read-char me))
  ({.read-all}    (__read-all me))
  ({.close}       (__close-port me))
  ({.read-line}
   ;; XXX return eof-object when at eof, right?
   (string<-list
    (begin reading ()
      (let ch me.read-char)
      (if (or (eof-object? ch) (= ch #\newline))
          '()
          (cons ch (reading))))))
  )

(make-trait sink-primitive me
  ({.display a}   (__display me a))
  ({.write a}     (__write me a))     ;XXX Scheme naming isn't very illuminating here
  ({.print a}     (a .selfie me))
  ({.output-string}                 ;XXX for string-sink only
   (__get-output-string me))
  )

(make-trait term-primitive me
  ({.tag}         (__term-tag me))
  ({.arguments}   (__term-arguments me))
  ({.selfie sink}
   (sink .display "{")
   (sink .print me.tag)
   (for each! ((arg me.arguments))
     (sink .display " ")
     (sink .print arg))
   (sink .display "}"))
  ({.compare t}
   (`(,me.tag ,@me.arguments) .compare `(,t.tag ,@t.arguments))) ;XXX untested
  )

(make-trait void-primitive me
  ;; A Gambit type returned by some of the Gambit operations.
  )


;; Continuations

(to (__halt-cont)
  (make me {extending list-trait}
    ({.empty?}        #yes)
    ({.first}         (error "No more frames" me))
    ({.rest}          (error "No more frames" me))
    ({.selfie sink}   (sink .display "<halt-cont>"))))

(make-trait __cont-trait me
  ({.empty?}        #no)
  ({.selfie sink}   (sink .display "<cont>")) ;XXX more
  (message          (list-trait me message))) ;XXX use trait syntax instead

(to (__call-cont-standin-cont k message)
  (make {extending __cont-trait}
    ({.rest} k)
    ({.first} '("XXX still a hack"))))

(to (__match-clause-cont k pat-r body rest-clauses object script datum message)
  (make {extending __cont-trait}
    ({.rest} k)
    ({.first} `((^ ,body) ,@rest-clauses))))

(to (__ev-trait-cont k r name trait clauses)
  (make {extending __cont-trait}
    ({.rest} k)
    ({.first} `(make ,name ,trait ^
                 ,@clauses))))

(to (__ev-make-cont k name stamp-val r clauses)
  (make {extending __cont-trait}
    ({.rest} k)
    ({.first} `(make ,name ^ #no   ; XXX as above
                 ,@clauses))))

(to (__ev-do-rest-cont k r e2)
  (make {extending __cont-trait}
    ({.rest} k)
    ({.first} e2)))

(to (__ev-let-match-cont k r p)
  (make {extending __cont-trait}
    ({.rest} k)
    ({.first} `(<match> ,p))))          ;XXX lousy presentation

(to (__ev-let-check-cont k val)
  (make {extending __cont-trait}
    ({.rest} k)
    ({.first} `(<assert-matched-then> ',val))))

(to (__ev-arg-cont k r e2)
  (make {extending __cont-trait}
    ({.rest} k)
    ({.first} `(^ ,e2))))

(to (__ev-call-cont k receiver)
  (make {extending __cont-trait}
    ({.rest} k)
    ({.first} `(call ',receiver ^))))

(to (__ev-rest-args-cont k es r vals)
  (make {extending __cont-trait}
    ({.rest} k)
    ({.first}
     (to (quotify v) `',v)
     `(,@(each quotify (reverse vals)) ^ ,@es))))

(to (__ev-tag-cont k tag)
  (make {extending __cont-trait}
    ({.rest} k)
    ({.first} `{,tag ^^^})))

(to (__ev-and-pat-cont k r subject p2)
  (make {extending __cont-trait}
    ({.rest} k)
    ({.first} `(<and-match?> ,p2))))

(to (__ev-view-call-cont k r subject p)
  (make {extending __cont-trait}
    ({.rest} k)
    ({.first} `(: _ ^ ,p))))

(to (__ev-view-match-cont k r p)
  (make {extending __cont-trait}
    ({.rest} k)
    ({.first} p)))

(to (__ev-match-rest-cont k r subjects ps)
  (make {extending __cont-trait}
    ({.rest} k)
    ({.first} `(<all-match?> ,@ps))))
