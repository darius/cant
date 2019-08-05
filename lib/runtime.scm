;; This special source file must be written to load without actually
;; invoking any primitive object, because this code *defines* their
;; scripts. Only after this file is loaded will the definitions get
;; magically connected to the primitives.

;; That works out because the top level here is just definitions, with
;; no top-level actions.

;; There's also a definition of map<-, needed to implement (export ...).

;; Aaand this includes further definitions used by the above-needed
;; definitions, transitively.

(make-trait miranda-trait me
  (to (_ .selfie sink) (sink .display (__depict me)))
  (to message (error "Match failure" me message)))

;; Generic map trait
;; requires:
;;  .get key default
;;  .count -- hm, we could impl as .items.count
;;  .items

;; TODO untested, unused yet
;; TODO mutable-map trait?

;(make missing)
;; Problem: (map .get key missing) might save `missing` for later.
;; For now, we'll make a new `missing` every time, though that's
;; uglyish and kinda expensive.

(make-trait map-trait map
  (to (_ key)
    (make missing)
    (let answer (map .get key missing))
    (if (= answer missing)
        (error "Missing key" map key)
        answer))
  (to (_ .get key)
    (map .get key #no))
  (to (_ .maps? key)
    (make missing)
    (not= (map .get key missing) missing))
  (to _.empty? (= map.count 0))  ; or map.items.empty? - is that better?
  (to _.keys   (each _.first map.items))
  (to _.values (each (on (`(,_ ,v)) v) map.items))
  (to (_ .find? value)
    (map.values .find? value))
  (to (_ .find value default)
    (begin searching ((items map.items))
      (if items.empty?
          default
          (be items.first
            (`(,k ,v) (if (= v value) k (searching items.rest)))
            (else (searching items.rest))))))
  (to (_ .find value)
    (make missing)
    (let key (map .find value missing))
    (when (= key missing)
      (error "Missing value" value))
    key)
  (to _.copy
    (map<- map.items))
  (to (_ .intersects? map2)
    ;; TODO: maybe iterate over the one with smaller .count ?
    (for some ((k map.keys))
      (map2 .maps? k)))
  (to (_ .disjoint? map2)                    ;too trivial?
    (not (map .intersects? map2)))
  (to _.domain
    (set<-list map.keys))   ;TODO usually worth specializing
  (to _.range  ; TODO rename range<- to something else
    (set<-list map.values))
  (to _.inverse
    (let inverse (map<-))
    (for each! ((`(,k ,v) map.items))
      (when (inverse .maps? v)
        (error ".inverse of noninvertible map" map))  ; or just allow it?
      (inverse .set! v k))
    inverse)

  ;; What's the right definition & interface for these for maps?
  ;; TODO also, rename to .or, .and ?
  (to (_ .union other)
    (error "unimplemented .union"))
  (to (_ .intersect other)                 
    (error "unimplemented .intersect"))
  (to (_ .difference other)
    (error "unimplemented .difference"))

;;  (to (_ .compare xs)
;;  (to (_ .slice keys)
  )

(make-trait list-trait list
  (to (_ i)
    (if (= i 0)
        list.first
        (list.rest (- i 1))))
  (to _.empty?
    (= 0 list.count)) ;N.B. these default implementations are circular
  (to _.first
    (list 0))
  (to _.rest
    (list .slice 1))
  (to _.count
    ;; TODO non-tail-recursive would be more OO in style. Go back to that?
    (begin counting ((list list) (count 0))
      (if list.empty?
          count
          (counting list.rest (+ count 1)))))
  (to (_ .slice i)
    (surely (<= 0 i))
    (hm (if (= i 0) list)
        (if list.empty? list)
        (else (list.rest .slice (- i 1)))))
  (to (_ .slice i bound)     ;XXX result is a link-list; be more generic?
    (surely (<= 0 i))
    (hm (if list.empty? list)
        (if (<= bound i) '())
        (if (= i 0) (link list.first (list.rest .slice 0 (- bound 1))))
        (else (list.rest .slice (- i 1) (- bound 1)))))
  (to (_ .chain seq)                         ;TODO self if seq is ()
    (if list.empty?
        seq
        (link list.first (list.rest .chain seq))))
  (to (_ .compare xs)
    ;; N.B. mutable arrays compare by this method, so it's really a comparison as of right now
    (hm (if list.empty? (if xs.empty? 0 -1))
        (if xs.empty? 1)
        (else (be (list.first .compare xs.first)
                (0 (list.rest .compare xs.rest))
                (d d)))))
  ;; A sequence is a kind of collection. Start implementing that:
  (to _.keys
    (range<- list.count)) ;TODO move this impl to array-trait; here, enum lazily.
  (to _.values
    list)
  (to _.items
    (enumerate list))
  (to (_ .get key default)
    (if (count? key)
        (begin walking ((k key) (xs list))
          (hm (if xs.empty? default)
              (if (= k 0) xs.first)
              (else (walking (- k 1) xs.rest))))
        default))
  (to (_ .maps? key)
    (and (not list.empty?)
         (or (= 0 key)
             (and (< 0 key)
                  (list.rest .maps? (- key 1))))))
  (to (_ .find value default)    ;; XXX update the other collections to have this too
    (begin looking ((i 0) (values list))
      (hm (if values.empty? default)
          (if (= value values.first) i)
          (else (looking (+ i 1) values.rest)))))
  (to (_ .find value)
    (be (list .find value #no)
      (#no (error "Missing value" value))
      (key key)))
  (to (_ .find? value)
    (be (list .find value #no)
      (#no #no)
      (_ #yes)))
  (to (_ .last)
    (let rest list.rest)
    (if rest.empty? list.first rest.last))
  (to (_ .repeat n)
   ;;TODO a method to get an empty seq of my type; and then factor out duplicate code
    (be n
      (0 '())             
      (_ (call chain (for each ((_ (range<- n)))
                       list)))))
  (to _.maybe  ;; TODO an experiment TODO could be defined on maps in general too
    (if list.empty?
        #no
        (do (unless list.rest.empty?
              (error "Tried to convert to maybe from count >1" list))
            list.first)))
  (to _.only  ;; TODO an experiment TODO could be defined on maps in general too
    (when list.empty?
      (error "Tried to .only from empty" list))
    (unless list.rest.empty?
      (error "Tried to .only from count >1" list))
    list.first)
  (to message
    (map-trait list message)))

(make-trait claim-primitive me
  (to _.count       (if me 1 0))
  (to (_ .selfie sink) (sink .display (if me "#yes" "#no")))
  (to (_ .compare a)
    (hm (and (claim? a))
        (if (= me a) 0)
        (if me       1)
        (else       -1)))
  )

(make-trait procedure-primitive me
  )

(make-trait number-primitive me
  (to (_ .+ a)         (__+ me a))
  (to (_ .- a)         (__- me a))
  (to (_ .* a)         (__* me a))
  (to (_ .compare a)   (__number-compare me a))
  (to (_ .quotient b)  (__quotient me b))
  (to (_ .remainder b) (__remainder me b))
  (to (_ .modulo b)    (__modulo me b))
  (to (_ .*/mod m d)   (__*/mod me m d))
  (to (_ ./mod d)      (__*/mod me 1 d))
  (to (_ .<< b)        (__bit-<<  me b))
  (to (_ .>> b)        (__bit->>  me b))
  (to _.not            (__bit-not me))
  (to (_ .and b)       (__bit-and me b))
  (to (_ .or b)        (__bit-or  me b))
  (to (_ .xor b)       (__bit-xor me b))
  (to (_ .to< b)       (range<- me b))
  (to (_ .to b)        (range<- me (+ b 1)))
  (to (_ .span n)      (range<- me (+ me n)))
  (to _.even?          (surely (integer? me)) (= 0 (me .modulo 2)))
  (to _.odd?           (surely (integer? me)) (not= 0 (me .modulo 2)))
  (to (_ .divides? b)  (surely (integer? me)) (= 0 (b .modulo me)))
  ;; XXX sketchy support for 32-bit word ops:
  (to (_ .u+ a)        (__u+ me a))
  (to (_ .u- a)        (__u- me a))
  (to (_ .u/ a)        (__u/ me a))
  (to (_ .u>> a)       (__u>> me a))
  (to (_ .u<< a)       (__u<< me a))
  (to (_ .s+ a)        (__s+ me a))
  (to (_ .s* a)        (__s* me a))
  )

(make-trait symbol-primitive me
  (to _.method         (on (actor @arguments) ;TODO experiment; vs. method<- in stdlib
                         (call actor (term<- me arguments))))
  (to _.name           (__symbol->string me))
  (to (_ .compare a)   (and (symbol? a)
                            (me.name .compare a.name)))
  (to (_ .selfie sink) (sink .display me.name))
  ;; TODO experiment:
  (to _.term<-         (on (@arguments) (term<- me arguments)))
  ;; Some silly conveniences for sturm:
  (to _.lowercase      (symbol<- me.name.lowercase))
  (to _.uppercase      (symbol<- me.name.uppercase))
  )

(make-trait nil-primitive me
  (to _.empty?         #yes)
  (to _.first          (error "Empty list" '.first))
  (to _.rest           (error "Empty list" '.rest))
  (to _.count          0)
  (to (_ i)            (error "Empty list" 'nth i))
  (to (_ .chain a)     a)
  (to (_ .selfie sink) (sink .display "()"))
  (to message          (list-trait me message))) ;XXX use trait syntax instead

(make-trait link-primitive me
  (to _.empty?      #no)
  (to _.first       (__car me))
  (to _.rest        (__cdr me))
  (to _.count       (__length me))
  (to (_ i)         (__list-ref me i))    ;XXX just use the trait method? then can e.g. mix lazy and eager list nodes
  (to (_ .chain a)  (__append me a))
  (to (_ .selfie sink)
    (be me
      (`(quote ,x)
       (sink .display "'")
       (sink .print x))
      (else
       (sink .display "(")
       (sink .print me.first)
       (begin printing ((r me.rest))
         (hm (when (link? r)
               (sink .display " ")
               (sink .print r.first)
               (printing r.rest))
             (when (null? r) 'ok)
             (else
               (sink .display " . ")       ;XXX we're not supporting this in read, iirc
               (sink .print r))))
       (sink .display ")"))))
  (to message
    (list-trait me message))) ;XXX use trait syntax instead

(make-trait array-trait me
  (to (_ .slice i)
    (me .slice i me.count))
  (to (_ .slice i bound)                     ;XXX untested
    (let v (array<-count (- bound i)))
    (v .move! 0 me i bound)
    v)
  (to _.last
    (me (- me.count 1)))
  (to (_ .copy! v)
    (me .move! 0 v 0 v.count))
  (to (_ .move! dst source lo bound)
    ;; TODO no-op if in range and (me,dst) == (source,lo)
    (let lo->dst (- dst lo))
    (for each! ((i (if (<= dst lo)
                       (range<- lo bound)
                       (range<- (- bound 1) lo -1))))
      (me .set! (+ i lo->dst)
          (source i))))
  (to _.values
    (each me (range<- me.count)))   ;TODO cheaper to represent by self -- when can we get away with that?
  (to _.items
    (for each ((i (range<- me.count)))
      `(,i ,(me i))))
  (to (_ .get key default)
    (hm (unless (count? key) default)
        (if (<= me.count key) default)
        (else (me key))))
  (to (_ .swap! i j)
    (let t (me i))
    (me .set! i (me j))
    (me .set! j t))
  (to message
    (list-trait me message))) ;XXX use trait syntax instead

(make-trait array-primitive me
  (to _.empty?        (= 0 me.count))
  (to _.first         (me 0))
  (to _.rest          (me .slice 1))
  (to (_ .set! i val) (__vector-set! me i val))
  (to _.count         (__vector-length me))
  (to (_ i)           (__vector-ref me i))
  (to (_ .maps? i)    (__vector-maps? me i))
  (to (_ .chain v)    (__vector-append me v))
  (to _.values        (__vector->list me))
  (to (_ .slice i)    (__subvector me i me.count))
  (to (_ .slice i j)  (__subvector me i j))
  (to (and (_ .move! dst source lo bound)
           message)
    ;; Block-copy source[lo..bound) to me[dst..dst+(bound-lo)).
    (if (array? source)
        (__vector-move! me dst source lo bound)
        (array-trait me message)))
  (to _.copy          (__vector-copy me))
  (to (_ .selfie sink)
    (sink .display "[")
    (when (< 0 me.count)
      (sink .print (me 0))
      (for each! ((x ((__vector->list me) .rest)))
        (sink .display #\space)
        (sink .print x)))
    (sink .display "]"))
;   (sink .print (__vector->list me)))
  (to message
    (array-trait me message))) ;XXX use trait syntax instead

(make-trait string-primitive me
  (to _.empty?       (= 0 me.count))
  (to _.first        (me 0))
  (to _.rest         (me .slice 1))
  (to _.count        (__string-length me))
  (to (_ i)          (__string-ref me i))
  (to (_ .maps? i)   (__string-maps? me i))
  (to (_ .chain s)   (__string-append me s))
  (to (_ .slice i)   (__substring me i me.count))
  (to (_ .slice i j) (__substring me i j))
  (to (_ .compare s)
    (if (string? s)
        (__string-compare me s)          ; just a speedup
        (list-trait me (_ .compare s))))   ; but is this what we really want? (<=> "a" '(#\a))
  (to (_ .join ss)   ;should this be a function, not a method?
    (if ss.empty?
        ""
        (foldr1 (on (x y) (chain x me y)) ss))) ;XXX quadratic
  ;;XXX below mostly from list-trait, until .selfie
  (to _.keys         (range<- me.count))
  (to _.values       (list<-string me))
  (to _.items        (enumerate me))
  (to (_ .get key)   (me .get key #no))
  (to (_ .get key default)
    (if (me .maps? key)
        (me key)
        default))
  (to (_ .maps? key)                         ;XXX duplicate, see above
    (and (count? key) (< key me.count)))
  (to (_ .trim-left)
    (if me.empty?
        me
        (do (let c me.first)
            (if c.whitespace?
                me.rest.trim-left
                me))))
  (to _.trim-right
    (begin scanning ((i me.count))
      (if (= i 0)
          ""
          (do (let c (me (- i 1)))
              (if c.whitespace?
                  (scanning (- i 1))
                  (me .slice 0 i))))))
  (to _.trim
    me.trim-left.trim-right)
  (to _.split
    (begin splitting ((s me.trim-left))
      (if s.empty?
          '()
          (do (let limit s.count)
              (begin scanning ((i 1))
                (hm (if (= i limit) `(,s))
                    (if ((s i) .whitespace?)
                        (link (s .slice 0 i)
                              (splitting ((s .slice (+ i 1)) .trim-left))))
                    (else (scanning (+ i 1)))))))))
  (to (_ .split delimiter)
    ;; TODO deduplicate code
    ;; TODO define a strstr and use that
    (if me.empty?
        '()
        (begin splitting ((s me))
          (if s.empty?
              '("")
              (do (let limit s.count)
                  (begin scanning ((i 0))
                    (hm (if (= i limit) `(,s))
                        (if (= delimiter (s .slice i (+ i delimiter.count)))
                            (link (s .slice 0 i)
                                  (splitting (s .slice (+ i delimiter.count)))))
                        (else (scanning (+ i 1))))))))))
  (to _.lowercase (string<-list (for each ((c me)) c.lowercase)))
  (to _.uppercase (string<-list (for each ((c me)) c.uppercase)))
  (to _.capitalize (chain ((me .slice 0 1) .uppercase) (me .slice 1)))
  (to (_ .starts-with? s)
    (= (me .slice 0 s.count) s))   ;TODO more efficient
  (to (_ .replace pattern replacement) ;TODO more efficient
    ;; TODO unify the cases?
    (hm (if pattern.empty?
            (for foldr ((ch me) (rest replacement))
              (chain replacement (string<- ch) rest)))
        (else
         (let limit me.count)
         (string<-list
          ;; TODO define a strstr and use that
          (begin scanning ((i 0))
            (hm (if (= i limit) '())
                (if (= pattern (me .slice i (+ i pattern.count)))
                    (chain (list<-string replacement)
                           (scanning (+ i pattern.count))))
                (else (link (me i) (scanning (+ i 1))))))))))
  (to (_ .justify n)
    (me .justify n #\space))
  (to (_ .justify n pad)
    (if (< n 0)
        (me .left-justify (- n) pad)
        (me .right-justify n    pad)))
  (to (_ .left-justify n)
    (me .left-justify n #\space))
  (to (_ .left-justify n pad-char)
    (let pad (- n me.count))
    (if (<= pad 0)
        me
        (chain me ((string<- pad-char) .repeat pad))))
  (to (_ .right-justify n)
    (me .right-justify n #\space))
  (to (_ .right-justify n pad-char)
    (let pad (- n me.count))
    (if (<= pad 0)
        me
        (chain ((string<- pad-char) .repeat pad) me)))
  (to (_ .center n)
    (let pad (- n me.count))
    (if (<= pad 0)
        me
        (do (let half (pad .quotient 2))
            (chain (" " .repeat (- pad half))
                   me
                   (" " .repeat half)))))
  (to (_ .repeat n)
    (be n
      (0 "")
      (_ (call chain (for each ((_ (range<- n)))
                       me)))))
  (to (_ .format @arguments)
    (let sink (string-sink<-))
    (call format `{.to-sink ,sink ,me ,@arguments})
    sink.output-string)
  (to _.split-lines
    (let lines (me .split "\n"))
    ;; TODO ugly. This 'if' is needed because we want a final "\n" to
    ;; yield the same output as a string with no final "\n". N.B. while
    ;; that's convenient it's also information-destroying.
    (if (and (not lines.empty?) (= lines.last ""))
        (lines .slice 0 (- lines.count 1))
        lines))
  (to (_ .selfie sink)
    (sink .display #\")
    (for each! ((c me))
      (sink .display (be c            ;XXX super slow. We might prefer to use the Scheme built-in.
                       (#\\ "\\\\")
                       (#\" "\\\"")
                       (#\newline "\\n")
                       (#\tab     "\\t")
                       (#\return  "\\r")
                       ;; XXX escape the control chars
                       (_ c))))
    (sink .display #\"))
  (to message
    (list-trait me message))) ;XXX use trait syntax instead

(make-trait char-primitive me
  (to _.code          (__char->integer me))
  (to _.letter?       (__char-letter? me))
  (to _.digit?        (__char-digit? me))
  (to _.whitespace?   (__char-whitespace? me))
  (to _.alphanumeric? (or me.letter? me.digit?))
  (to _.lowercase?    (__char-lowercase? me))
  (to _.uppercase?    (__char-uppercase? me))
  (to _.lowercase     (__char-lowercase me))
  (to _.uppercase     (__char-uppercase me))
  (to (_ .compare c)  (__char-compare me c))
  (to (_ .+ n)   ;; Is this a good idea?
    (surely (integer? n) "Bad arg type" n)
    (char<- (+ me.code n)))
  (to (_ .- b)
    (be b
      ((? integer?) (char<- (- me.code b)))
      ((? char?)    (- me.code b.code))
      (_ (error "Bad arg type" b))))
  (to (_ .to< b)      (range<- me b))       ;These methods should be in a trait
  (to (_ .to b)       (range<- me (+ b 1))) ;if they're a good idea at all...
  (to (_ .span n)     (range<- me (+ me n)))
  )

;; TODO: should a box be a collection?
(make-trait box-primitive me
  (to _.^             (__box-value me))
  (to (_ .^= val)     (__box-value-set! me val))
  (to (_ .selfie sink)
    (sink .display "<box ")
    (sink .print me.^)
    (sink .display ">"))
  )

(make-trait source-primitive me
  (to _.read-char   (__read-char me))
  (to _.read-u8     (__get-u8 me))
  (to _.read-all    (__read-all me))
  (to _.close       (__close-port me))
  (to _.ready?      (__char-ready? me))
  (to _.read-line
    (let ch me.read-char)
    (if (eof? ch)
        ch
        (string<-list
         (begin reading ((ch ch))
           (if (or (eof? ch) (= ch #\newline))
               '()
               (link ch (reading me.read-char)))))))
  (to _.read-lines
    me.read-all.split-lines)  ;; TODO inefficient. also, maybe include the newlines?
  )

(make-trait sink-primitive me
  (to (_ .display a)   (__display a me))
  (to (_ .write-u8 u8) (__put-u8 me u8))
  (to (_ .print a)     (a .selfie me))
  (to _.close          (__close-port me))
  (to _.output-string                 ;XXX for string-sink only
    (__get-output-string me))
  )

(make-trait term-primitive me
  (to (_ receiver)     (call receiver me))
  (to _.tag            (__term-tag me))
  (to _.arguments      (__term-arguments me))
  (to (_ .selfie sink)
    (sink .display "{")
    (sink .print me.tag)
    (for each! ((arg me.arguments))
      (sink .display " ")
      (sink .print arg))
    (sink .display "}"))
  (to (_ .compare t)
    (`(,me.tag ,@me.arguments) .compare `(,t.tag ,@t.arguments))) ;XXX untested
  )

(make-trait void-primitive me
  ;; A Scheme type returned by some of the Scheme operations.
  )

(make-trait eof-primitive me
  ;; The eof object.
  )

(make-trait script-primitive me
  (to _.name    (__script-name me))
  (to _.trait   (__script-trait me))
  (to _.clauses (__script-clauses me))
  (to (_ .selfie sink)
    (sink .display "<script ")
    (sink .display me.name)
    (sink .display ">"))
  )

(make-trait cps-primitive me
  (to (_ .selfie sink)
    (sink .display "#<primitive ")
    (sink .display (__cps-primitive-name me))
    (sink .display ">"))
  )

(make-trait ejector-primitive me
  (to (_ .eject value)
    (__eject me value))
  (to (_ .selfie sink)
    (sink .display "#<ejector>"))
  )


;; Continuations

(to (__unexp e)         (unparse-exp (__expr e)))
(to (__unpat p)         (unparse-pat (__patt p)))
(to (__unclause clause) (unparse-clause (__clause clause)))

(to (__clause `(,p ,pv ,ev ,e))
  `(,(__patt p) ,pv ,ev ,(__expr e)))

(make-trait __halt-cont me
  (to _.empty?        #yes)
  (to _.first         (error "No more frames" me))
  (to _.rest          (error "No more frames" me))
  (to (_ .selfie sink)   (sink .display "<halt-cont>"))
  (to message (list-trait me message)))

(make-trait __cont-trait me   ;; For the non-halt cont types
  (to _.empty?        #no)
  (to _.rest          (__cont-next-cont me)) ;TODO
  (to (_ .selfie sink)   (sink .display "<cont>")) ;TODO at least give out the tag
  (to _.env
    ((__cont-data me) .first)) ; Commonly this, but sometimes needs to be overridden.
  (to message
    (list-trait me message))) ;XXX use trait syntax instead

(make-trait __match-clause-cont me
  (to _.first
    (let `(,pat-r ,body ,rest-clauses ,object ,script ,datum ,message) (__cont-data me))
    `((^ ,(__unexp (body 1)))
      ,@(each __unclause rest-clauses)))
  (to message
    (__cont-trait me message)))

(make-trait __ev-make-cont me
  (to _.first
    (let `(,r ,name ,clauses) (__cont-data me))
    `(make ,name ^
       ,@(each __unclause clauses)))
  (to message
    (__cont-trait me message)))

(make-trait __ev-do-rest-cont me
  (to _.first
    (let `(,r ,e2) (__cont-data me))
    (__unexp e2))
  (to message
    (__cont-trait me message)))

(make-trait __ev-let-match-cont me
  (to _.first
    (let `(,r ,p) (__cont-data me))
    `(<match> ,(__unpat p)))          ;XXX lousy presentation
  (to message
    (__cont-trait me message)))

(make-trait __ev-let-check-cont me
  (to _.first
    (let `(,val) (__cont-data me))
    `(<assert-matched-then> ',val))
  (to _.env
    '())
  (to message
    (__cont-trait me message)))

(make-trait __ev-arg-cont me
  (to _.first
    (let `(,r ,e2) (__cont-data me))
    `(^ ,(__unexp e2)))
  (to message
    (__cont-trait me message)))

(make-trait __ev-call-cont me
  (to _.first
    (let `(,receiver) (__cont-data me))
    `(call ',receiver ^))
  (to _.env
    '())
  (to message
    (__cont-trait me message)))

(make-trait __ev-rest-args-cont me
  (to _.first
    (let `(,r ,es ,vals) (__cont-data me))
    (to (quotify v) `',v)
    `(,@(each quotify (reverse vals)) ^ ,@(each __unexp es)))
  (to message
    (__cont-trait me message)))

(make-trait __ev-tag-cont me
  (to _.first
    (let `(,tag) (__cont-data me))
    `{,tag ^^^})
  (to _.env
    '())
  (to message
    (__cont-trait me message)))

(make-trait __ev-and-pat-cont me
  (to _.first
    (let `(,r ,subject ,p2) (__cont-data me))
    `(<and-match?> ,(__unpat p2)))
  (to message
    (__cont-trait me message)))

(make-trait __ev-view-call-cont me
  (to _.first
    (let `(,r ,subject ,p) (__cont-data me))
    `(? _ ^ ,(__unpat p)))
  (to message
    (__cont-trait me message)))

(make-trait __ev-view-match-cont me
  (to _.first
    (let `(,r ,p) (__cont-data me))
    (__unpat p))
  (to message
    (__cont-trait me message)))

(make-trait __ev-match-rest-cont me
  (to _.first
    (let `(,r ,subject ,ps) (__cont-data me))
    `(<all-match?> ,@(each __unpat ps)))
  (to message
    (__cont-trait me message)))

(make-trait __unwind-cont me
  (to _.first
    '<unwind>)                           ;TODO show more
  (to _.env
    '())
  (to message
    (__cont-trait me message)))

(make-trait __replace-answer-cont me
  (to _.first
    (let `(,value) (__cont-data me))
    `(<replace-answer> ',value))
  (to _.env
    '())
  (to message
    (__cont-trait me message)))


;; Hash-maps
;; This is defined in the runtime, here, because the form
;; (export foo bar) gets expanded into code like
;;   (map<- `((foo ,foo) (bar ,bar))) 
;; (but hygienic, when I get to fixing the current bad hygiene).

;; TODO:
;;   extend map-trait
;;   test deletion more
;;   nonlinear probing -- how about xor probing?
;;   preserving insertion order
;;   immutable snapshots
;;
;;   impl without a million boxes
;;   N.B. impl needs shared closures for efficiency
;;        (capacity, occupants, ..., hashmap)
;;   special-case impls for small maps and common-typed maps
;;   store hash codes instead of recomputing?
;;   etc.

(let map<-
  (hide

    (make none)
    (make deleted)

    (make map<-

      (to '()
        (let count (box<- 0))
        (let keys  (box<- (array<- none)))  ;; size a power of 2
        (let vals  (box<- (array<- #no)))   ;; same size

       ;; temp performance tracking
;;       (let n-places (box<- 0))
;;       (let n-probes (box<- 0))

        (to (capacity) keys.^.count)

        (to (occupants)
          (begin walking ((i (- (capacity) 1)))
            (hm (if (< i 0)       '())
                (do (let k (keys.^ i)))
                (if (= k none)    (walking (- i 1)))
                (if (= k deleted) (walking (- i 1)))
                (else             (link i (walking (- i 1)))))))

        (to (place key)
          (__place key keys.^ none deleted))

        (to (maybe-grow)
          (when (< (* 2 (capacity))
                   (* 3 count.^))
            (resize (* 2 (capacity)))))

        (to (resize new-capacity)
;;         (print `(resize ,new-capacity places ,n-places.^ probes ,n-probes.^
;;                         average ,(inexact<-exact (/ n-probes.^ (max 1 n-places.^)))))
;;         (n-places .^= 0)
;;         (n-probes .^= 0)
          (let old-keys keys.^)
          (let old-vals vals.^)
          (keys .^= (array<-count new-capacity none))
          (vals .^= (array<-count new-capacity))
          (for each! ((`(,i ,key) old-keys.items))
            (unless (or (= key none) (= key deleted))
              (let {missing-at j} (place key))
              (keys.^ .set! j key)
              (vals.^ .set! j (old-vals i)))))
       
        (make hashmap {extending map-trait}
          (to (_ key)
            (be (place key)
              ({at i} (vals.^ i))
              (_      (error "Missing key" hashmap key))))
          (to (_ .get key @(optional default))
            (be (place key)
              ({at i} (vals.^ i))
              (_      default)))
          (to (_ .set! key val)
            (be (place key)
              ({at i}
               (vals.^ .set! i val))
              ({missing-at i}
               (keys.^ .set! i key)
               (vals.^ .set! i val)
               (count .^= (+ count.^ 1))
               (maybe-grow))))
          (to (_ .maps? key)
            (be (place key)
              ({at _} #yes)
              (_      #no)))
          (to _.empty? (= count.^ 0))
          (to _.count  count.^)
          (to _.keys   (each keys.^ (occupants))) ;XXX lazy-map
          (to _.values (each vals.^ (occupants)))
          (to _.items
            (let ks keys.^)
            (let vs vals.^)
            (for each ((i (occupants)))
              `(,(ks i) ,(vs i))))
          (to (_ .get-set! key value<-)
            (be (place key)
              ({at i}
               (vals.^ i))
              ({missing-at _}
               (let value (value<-))
               ;; Alas, we can't just stick it in at i because (value<-)
               ;; might have changed things too:
               (hashmap .set! key value)
               value)))
          (to (_ .delete! key)
            (be (place key)
              ({at i}
               (keys.^ .set! i deleted)
               (count .^= (- count.^ 1))
               #no)
              (_ #no)))   ;XXX error instead? It is in Python.
          (to (_ .find? value)
            (hashmap.values .find? value))
          (to (_ .find value default)
            (let vs vals.^)
            (begin searching ((js (occupants)))  ;XXX should be lazy
              (hm (if js.empty? default)
                  (if (= value (vs js.first)) (keys.^ js.first))
                  (else (searching js.rest)))))
          (to _.clear!
            (count .^= 0)
            (keys .^= (array<- none))
            (vals .^= (array<- #no)))
          (to _.copy
            (map<- hashmap.items))
          (to (_ .selfie sink)
            (sink .display "#<hash-map (")
            (sink .print count.^)
            (sink .display ")>"))
          ))

      (to (_ a-list) ;TODO invent a concise constructor; frozen by default
        (let m (map<-))
        (for each! ((`(,k ,v) a-list))
          (m .set! k v))
        m))))

;; Sets via hashtable
;; TODO unify with hashmaps

(to (set<- @vals)            ;XXX this name is better saved for frozen sets
  (let s (hash-set<-))
  (s .add-all! vals)
  s)

(to (set<-list vals)            ;XXX this name is better saved for frozen sets
  (let s (hash-set<-))
  (s .add-all! vals)
  s)

(to (hash-set<-)                        ;XXX shouldn't be a global
  (let map (map<-)) ;TODO would be nice to avoid storing all the #yes values
  (make hash-set {extending map-trait}
    (to _.empty?           map.empty?)
    (to _.count            map.count)
    (to _.keys             map.keys)
    (to (_ .maps? key)     (map .maps? key))
    (to _.copy             (set<-list map.keys)) ;TODO tune
    (to (_ .add! key)      (map .set! key 1))    ;N.B. matching the bag type here
    (to (_ .add-all! vals) (for each! ((v vals)) (hash-set .add! v)))
    (to (_ .union! other)  (hash-set .add-all! other.keys))
    (to (_ .union other)
      (let result hash-set.copy)
      (result .union! other)
      result)
    (to (_ .intersect other)                 ;TODO rename to .and, etc., I guess
      (let result (set<-))
      (for each! ((x map.keys))
        (when (other .maps? x)
          (result .add! x)))
      result)
    (to (_ .difference other)
      (let result (set<-))
      (for each! ((x map.keys))
        (unless (other .maps? x)
          (result .add! x)))
      result)
    (to (_ .intersects? map2)
      (map .intersects? map2))
    (to _.clear!         map.clear!)
    (to (_ .get key)     (map .maps? key))
    (to (_ key)          (map .get key 0)) ;I'm not sure this is a good idea, but it's to match the bag type
    (to _.items          map.items)
    (to _.values         map.values)
    (to (_ .delete! key) (map .delete! key))
    (to _.total          map.count)        ;like bags again
    ;; XXX fill in rest of set interface (just the map interface, I guess)
    (to (_ .selfie sink)
      (sink .display "#<set")
      (sink .print map.keys)
      (sink .display ">"))
    ))


;; stdlib

(to (surely ok? @arguments)
  (unless ok?
    (call error (if arguments.empty? '("Assertion failed") arguments))))

(to (count? x)
  (and (integer? x) (<= 0 x)))

(to (not= x y)
  (not (= x y)))

(make +
  (to (_) 0)
  (to (_ a) a)
  (to (_ a b) (a .+ b))
  (to (_ a b @arguments)
    (foldl (on (x y) (x .+ y)) (a .+ b) arguments)))

(make *
  (to (_) 1)
  (to (_ a) a)
  (to (_ a b) (a .* b))
  (to (_ a b @arguments)
    (foldl (on (x y) (x .* y)) (a .* b) arguments)))

(make -
  (to (_ a) (0 .- a))
  (to (_ a b) (a .- b))
  (to (_ a b @arguments)
    (foldl (on (x y) (x .- y)) (a .- b) arguments)))

(make-trait transitive-comparison compare?
  (to (_ x @xs)
    (begin comparing ((x0 x) (xs xs))
      (be xs
        (`() #yes)
        (`(,x1 ,@rest) (and (compare? x0 x1)
                            (comparing x1 rest)))))))

(make <   {extending transitive-comparison} (to (_ a b)      (= (compare a b) -1)))
(make <=  {extending transitive-comparison} (to (_ a b) (not (= (compare a b)  1))))
(make <=> {extending transitive-comparison} (to (_ a b)      (= (compare a b)  0))) ; XXX better name?
(make >=  {extending transitive-comparison} (to (_ a b) (not (= (compare a b) -1))))
(make >   {extending transitive-comparison} (to (_ a b)      (= (compare a b)  1)))

(to (compare a b)
  (let result (a .compare b))
  (if (comparison? result) result (error "Incomparable" a b)))

(to (comparison? x)
  (be x
    (-1 #yes)
    ( 0 #yes)
    (+1 #yes)
    (_  #no)))


;;XXX so should some of these be in list-trait?

(to (as-list seq)            ;XXX naming convention for coercions?
  (if (list? seq)
      seq
      (begin copying ((seq seq))
        (if seq.empty?
            '()
            (link seq.first (copying seq.rest))))))

(to (string<-list chars) (__string<-list (as-list chars)))
(to (array<-list xs)     (__array<-list (as-list xs)))

(to (reverse xs)
  (for foldl ((ys '()) (x xs))
    (link x ys)))

(to (foldl f z xs)
  (if xs.empty?
      z
      (foldl f (f z xs.first) xs.rest)))

(to (foldr f xs z)     ;TODO rename since args are in nonstandard order
  (if xs.empty?
      z
      (f xs.first (foldr f xs.rest z))))

(to (foldr1 f xs)
  (let tail xs.rest)
  (if tail.empty?
      xs.first
      (f xs.first (foldr1 f tail))))

(make each
  (to (_ f xs)
    (for foldr ((x xs) (ys '()))
      (link (f x) ys)))
  (to (_ f @lists)
    (for each ((args (call zip lists)))
      (call f args))))

(make zip
  (to (_ xs ys)                           ;specialized for speed
    (to (mismatch)
      (error "zip: mismatched arguments" xs ys))
    (begin zipping ((xs xs) (ys ys))
      (hm (if xs.empty? (if ys.empty? '() (mismatch)))
          (if ys.empty? (mismatch))
          (else `((,xs.first ,ys.first)
                  ,@(zipping xs.rest ys.rest))))))
  (to (_ @lists)  ; ugly
    (transpose lists)))

;; TODO: name it (zip @rows) instead, like Python?
(to (transpose rows)
  (if (every _.empty? rows)   ; and make it (some _.empty? rows)?
      '()
      `(,(each _.first rows)
        ,@(transpose (each _.rest rows)))))

(to (gather f xs)
  (for foldr ((x xs) (gathered '()))
    (chain (f x) gathered)))

(to (those keep? xs)
  (for foldr ((x xs) (kept '()))
    (if (keep? x) (link x kept) kept)))

(to (yeahs maybe xs)             ;TODO is this worth defining? good name?
  (those identity (each maybe xs)))

(to (identity x)
  x)

(to (list<- @arguments)
  arguments)

(make chain
  (to (_) '())
  (to (_ xs) xs)
  (to (_ xs ys) (xs .chain ys))
  (to (_ @arguments)
    (foldr1 (on (xs ys) (xs .chain ys)) arguments)))

(to (some pass? xs)
  (and (not xs.empty?)
       (or (pass? xs.first)
           (some pass? xs.rest))))

(to (every pass? xs)
  (or xs.empty?
      (and (pass? xs.first)
           (every pass? xs.rest))))

(to (each! f xs)
  (unless xs.empty?
    (f xs.first)
    (each! f xs.rest)))

(make range<-
  (to (_ first limit)
    (if (<= limit first)
        '()
        (make range {extending list-trait}
          (to _.empty? #no)
          (to _.first  first)
          (to _.rest   (range<- (+ first 1) limit))
          (to _.count  (- limit first))
          (to (_ i)
            (if (not (integer? i))
                (error "Key error" range i)
                (do (let j (+ first i))
                    (if (and (<= first j) (< j limit))
                        j
                        (error "Out of range" range i)))))
          (to (_ .maps? i)
            (and (integer? i)
                 (do (let j (+ first i))
                     (and (<= first j) (< j limit)))))
          ;; TODO: .compare
          )))
  (to (_ limit)
    (range<- 0 limit))
  (to (_ first limit stride)
    ;; TODO factor the code better
    (hm (if (< 0 stride)
            (if (<= limit first)
                '()
                (make range {extending list-trait}
                  (to _.empty? #no)
                  (to _.first  first)
                  (to _.rest   (range<- (+ first stride) limit stride))
                  (to (_ i)
                    (error "TODO" range `(,i)))
                  (to (_ .maps? i)
                    (error "TODO" range (_ .maps? i)))
                  )))
        (if (< stride 0)
            (if (< first limit)
                '()
                (make range {extending list-trait}
                  (to _.empty? #no)
                  (to _.first  first)
                  (to _.rest   (range<- (+ first stride) limit stride))
                  (to (_ i)
                    (error "TODO" range `(,i)))
                  (to (_ .maps? i)
                    (error "TODO" range (_ .maps? i)))
                  )))
        (else
          (error "Zero stride" first limit stride)))))

(make enumerate
  (to (_ xs)
    (enumerate xs 0))
  (to (_ xs i)
    (if xs.empty?
        '()
        (make enumeration {extending list-trait}
          (to _.empty? #no)
          (to _.first  `(,i ,xs.first))
          (to _.rest   (enumerate xs.rest (+ i 1)))))))

(to (array<- @elements)
  (array<-list elements))

(to (string<- @chars)
  (string<-list chars))

(to (with-output-string take-sink)             ;TODO rename
  (let sink (string-sink<-))
  (take-sink sink)
  sink.output-string)


;; (Roughly) undo parse-exp and parse-pat.
;; Really we should track source-position info instead, and report that.
;; This is just to make debugging less painful till then.

(let (list<- unparse-exp unparse-pat unparse-clause)
  (hide

    (to (unparse-exp e)
      (be e.term
        ({constant c}
         (if (self-evaluating? c) c `',c))
        ({variable v}
         v)
        ({make name stamp trait clauses}
         (unparse-make name stamp trait clauses))
        ({do e1 e2}
         (unparse-do e1 e2))
        ({let p e}
         `(let ,(unparse-pat p) ,(unparse-exp e)))
        ({call e1 e2}
         (be e2.term
           ({list operands}
            `(,(unparse-exp e1) ,@(each unparse-exp operands)))
           ({term (? cue? cue) operands}
            `(,(unparse-exp e1) ,cue ,@(each unparse-exp operands)))
           (_
            `(call ,(unparse-exp e1) ,(unparse-exp e2)))))
        ({term tag es}
         (term<- tag (each unparse-exp es)))
        ({list es}
         `(list<- ,@(each unparse-exp es))))) ;XXX unhygienic

    (to (unparse-do e1 e2)
      (let es
        (begin unparsing ((tail e2))
          (be tail.term
            ({do e3 e4} (link e3 (unparsing e4)))
            (_ `(,tail)))))
      `(do ,@(each unparse-exp (link e1 es))))

    (to (unparse-make name stamp trait-term clauses)
      (surely (= {constant #no} stamp.term)) ;XXX
      `(make ,name
         ,@(be trait-term.term
             ({constant #no} '())
             (trait-e `({extending ,(unparse-exp trait-e)})))
         ,@(each unparse-clause clauses)))

    (to (unparse-clause `(,p ,p-vars ,e-vars ,e))
      `(,(unparse-pat p) ,(unparse-exp e)))

    (to (self-evaluating? x)            ;TODO this is already defined in terp/parse.scm
      (or (claim? x)
          (number? x)
          (char? x)
          (string? x)))

    (to (unparse-pat pat)
      ;; XXX these need updating to the newer pattern syntax
      (be pat.term
        ({constant-pat c}
         (if (self-evaluating? c) c `',c))
        ({any-pat}
         '_)
        ({variable-pat v}
         v)
        ({term-pat tag ps}
         (term<- tag (each unparse-pat ps)))
        ({list-pat ps}
         (each unparse-pat ps))
        ({and-pat p1 p2}
         `(<and-pat> ,(unparse-pat p1) ,(unparse-pat p2)))
        ({view-pat e p}
         `(<view-pat> ,(unparse-exp e) ,(unparse-pat p)))))

    (list<- unparse-exp unparse-pat unparse-clause)))


;; printf-ish thing. TODO do something completely different?
(let format
  (hide

    (make format
      (to (_ format-string @arguments)
        (scanning out format-string arguments))
      (to (_ .to-sink sink format-string @arguments)
        (scanning sink format-string arguments)))

    ;;TODO actually design the format language

    (to (scanning sink s args)
      (if s.empty? 
          (unless args.empty?
            (error "Leftover arguments" args))
          (be s.first
            (#\~
             (let ss s.rest)
             (if (ss .starts-with? "-")
                 (parse sink ss.rest -1 #no args)
                 (parse sink ss #no #no args)))
            (ch
             (sink .display ch)
             (scanning sink s.rest args)))))

    (to (parse sink s sign width args)
      (if (s .starts-with? "0")
          (parsing sink s.rest #\0     sign width args)
          (parsing sink s      #\space sign width args)))

    (to (parsing sink s pad sign width args)
      (when s.empty?
        (error "Incomplete format")) ;TODO report the format-string
      (be s.first
        (#\w
         (maybe-pad sink pad sign width (_ .print args.first))
         (scanning sink s.rest args.rest))
        (#\d
         (maybe-pad sink pad sign width (_ .display args.first))
         (scanning sink s.rest args.rest))
        (#\~
         (sink .display "~")
         (scanning sink s.rest args))
        ((? _.digit? ch)
         (let digit (- ch.code 48))
         (parsing sink s.rest pad sign      ;TODO testme with a multidigit width
                  (+ (if width (* 10 width) 0)
                     digit)
                  args))
        (#\x  ; hex number, XXX works wrong on negative numbers
         (maybe-pad sink pad sign width {.display ((string<-number args.first 16) .lowercase)})
         (scanning sink s.rest args.rest))
        (_
         (error "Bad format string" s))))

    (to (maybe-pad sink pad sign width message)
      (hm (when width
            (let string (with-output-string message))
            (let w (if sign (* sign width) width))
            (sink .display (string .justify w pad)))
          (when sign
            (error "Missing width in format string"))
          (else
            (call sink message))))

    format))
