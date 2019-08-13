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
  (to _.none?  (= map.count 0))  ; or map.items.none? - is that better?
  (to _.some?  (not map.none?))
  (to _.keys   (each _.first map.items))
  (to _.values (each (on (`(,_ ,v)) v) map.items))
  (to (_ .find? value)
    (map.values .find? value))
  (to (_ .find value default)
    (begin searching ((items map.items))
      (if items.none?
          default
          (may items.first
            (be `(,k ,v) (if (= v value) k (searching items.rest)))
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
        (list.rest i.-)))
  (to _.none?
    (= 0 list.count)) ;N.B. these default implementations are circular
  (to _.first
    (list 0))
  (to _.rest
    (list .slice 1))
  (to _.count
    ;; TODO non-tail-recursive would be more OO in style. Go back to that?
    (begin counting ((remainder list) (count 0))
      (if remainder.none?              ;XXX why can't this be .none?
          count
          (counting remainder.rest count.+))))
  (to (_ .slice i)
    (surely (<= 0 i))
    (hm (if (= i 0) list)
        (if list.none? list)
        (else (list.rest .slice i.-))))
  (to (_ .slice i bound)     ;XXX result is a link-list; be more generic?
    (surely (<= 0 i))
    (hm (if list.none? list)
        (if (<= bound i) '())
        (if (= i 0) (link list.first (list.rest .slice 0 bound.-)))
        (else (list.rest .slice i.- bound.-))))
  (to (_ .chain seq)                         ;TODO self if seq is ()
    (if list.none?
        seq
        (link list.first (list.rest .chain seq))))
  (to (_ .compare xs)
    ;; N.B. mutable arrays compare by this method, so it's really a comparison as of right now
    (hm (if list.none? (if xs.none? 0 -1))
        (if xs.none? 1)
        (else (may (list.first .compare xs.first)
                (be 0 (list.rest .compare xs.rest))
                (be d d)))))
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
          (hm (if xs.none? default)
              (if (= k 0) xs.first)
              (else (walking k.- xs.rest))))
        default))
  (to (_ .maps? key)
    (and list.some?
         (or (= 0 key)
             (and (< 0 key)
                  (list.rest .maps? key.-)))))
  (to (_ .find value default)    ;; XXX update the other collections to have this too
    (begin looking ((i 0) (values list))
      (hm (if values.none? default)
          (if (= value values.first) i)
          (else (looking i.+ values.rest)))))
  (to (_ .find value)
    (may (list .find value #no)
      (be #no (error "Missing value" value))
      (be key key)))
  (to (_ .find? value)
    (may (list .find value #no)
      (be #no #no)
      (else   #yes)))
  (to (_ .last)
    (let rest list.rest)
    (if rest.none? list.first rest.last))
  (to (_ .prefix? p)
    (= (list .slice 0 p.count) p))   ;TODO more efficient
  (to (_ .repeat n)
    ;;TODO a method to get an empty seq of my type; and then factor out duplicate code
    (may n
      (be 0 '())             
      (else (chain @(for each ((_ (range<- n)))
                      list)))))
  (to _.maybe  ;; TODO an experiment TODO could be defined on maps in general too
    (if list.none?
        #no
        (do (unless list.rest.none?
              (error "Tried to convert to maybe from count >1" list))
            list.first)))
  (to _.only  ;; TODO an experiment TODO could be defined on maps in general too
    (when list.none?
      (error "Tried to .only from empty" list))
    (unless list.rest.none?
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
  (to (_ .to b)        (range<- me b.+))
  (to (_ .span n)      (range<- me (+ me n)))
  (to _.even?          (surely (integer? me)) (= 0 (me .modulo 2)))
  (to _.odd?           (surely (integer? me)) (not= 0 (me .modulo 2)))
  (to (_ .divides? b)  (surely (integer? me)) (= 0 (b .modulo me)))
  (to _.+              (__+ me 1))      ;experiment
  (to _.-              (__- me 1))      ;experiment
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
  (to _.name           (__symbol->string me))
  (to (_ .compare a)   (and (symbol? a)
                            (me.name .compare a.name)))
  (to (_ .selfie sink) (sink .display me.name))
  ;; TODO experiment:
  (to _.term<-         (on (@arguments) (term<- me arguments)))
  (to _.method         (on (actor @arguments) ;TODO experiment; vs. method<- in stdlib
                         (call actor (term<- me arguments))))
  ;; Some silly conveniences for sturm:
  (to _.lowercase      (symbol<- me.name.lowercase))
  (to _.uppercase      (symbol<- me.name.uppercase))
  )

(make-trait nil-primitive me
  (to _.none?          #yes)
  (to _.first          (error "Empty list" '.first))
  (to _.rest           (error "Empty list" '.rest))
  (to _.count          0)
  (to (_ i)            (error "Empty list" 'nth i))
  (to (_ .chain a)     a)
  (to (_ .selfie sink) (sink .display "()"))
  (to message          (list-trait me message))) ;XXX use trait syntax instead

(make-trait link-primitive me
  (to _.none?       #no)
  (to _.first       (__car me))
  (to _.rest        (__cdr me))
  (to _.count       (__length me))
  (to (_ i)         (__list-ref me i))    ;XXX just use the trait method? then can e.g. mix lazy and eager list nodes
  (to (_ .chain a)  (__append me a))
  (to (_ .selfie sink)
    (may me
      (be `(quote ,x)
        (sink .display "'")
        (sink .write x))
      (else
       (sink .display "(")
       (sink .write me.first)
       (begin printing ((r me.rest))
         (hm (when (link? r)
               (sink .display " ")
               (sink .write r.first)
               (printing r.rest))
             (when (null? r) 'ok)
             (else
               (sink .display " . ")       ;XXX we're not supporting this in read, iirc
               (sink .write r))))
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
    (me me.count.-))
  (to (_ .copy! v)
    (me .move! 0 v 0 v.count))
  (to (_ .move! dst source lo bound)
    ;; TODO no-op if in range and (me,dst) == (source,lo)
    (let lo->dst (- dst lo))
    (for each! ((i (if (<= dst lo)
                       (range<- lo bound)
                       (range<- bound.- lo -1))))
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
  (to _.none?         (= 0 me.count))   ;redundant definition(s) for speed
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
  (to (_ .update key f)         ;TODO define in a mutable-map-trait ?
    (let value (f (me key)))
    (me .set! key value)
    value)
  (to (_ .selfie sink)
    (sink .display "[")
    (when (< 0 me.count)
      (sink .write (me 0))
      (for each! ((x ((__vector->list me) .rest)))
        (sink .display #\space)
        (sink .write x)))
    (sink .display "]"))
;   (sink .write (__vector->list me)))
  (to message
    (array-trait me message))) ;XXX use trait syntax instead

(make-trait string-primitive me
  (to _.none?        (= 0 me.count))
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
    (if ss.none?
        ""
        ;; De-quadratified (foldr1 (on (x y) (chain x me y)) ss)
        (do (let mine me.values)
            (string<-list (foldr1 (on (s chars)
                                    (s.values .chain (mine .chain chars.values)))
                                  ss)))))
  (to _.values       (list<-string me))
  (to (_ .get key)   (me .get key #no)) ;TODO duplicated because delegation is slow
  (to (_ .get key default)      ;TODO could be shared with array-trait
    (if (me .maps? key)
        (me key)
        default))
  (to (_ .trim-left)
    (let limit me.count)
    (begin scanning ((i 0))
      (hm (when (= i limit)
            "")
          (do (let c (me i)))
          (unless c.whitespace?
            (me .slice i))
          (else (scanning i.+)))))
  (to _.trim-right
    (begin scanning ((i me.count))
      (hm (when (= i 0)
            "")
          (do (let c (me i.-)))
          (unless c.whitespace?
            (me .slice 0 i))
          (else (scanning i.-)))))
  (to _.trim
    me.trim-left.trim-right)
  (to _.split
    ;; TODO dequadratify
    (begin splitting ((s me.trim-left))
      (if s.none?
          '()
          (do (let limit s.count)
              (begin scanning ((i 1))
                (hm (if (= i limit) `(,s))
                    (if ((s i) .whitespace?)
                        (link (s .slice 0 i)
                              (splitting ((s .slice i.+) .trim-left))))
                    (else (scanning i.+))))))))
  (to (_ .split delimiter)
    ;; TODO deduplicate code
    ;; TODO define a strstr and use that
    (if me.none?
        '()
        (begin splitting ((s me))
          (if s.none?
              '("")
              (do (let limit s.count)
                  (begin scanning ((i 0))
                    (hm (if (= i limit) `(,s))
                        (if (= delimiter (s .slice i (+ i delimiter.count)))
                            (link (s .slice 0 i)
                                  (splitting (s .slice (+ i delimiter.count)))))
                        (else (scanning i.+)))))))))
  (to _.lowercase (string<-list (each _.lowercase me)))
  (to _.uppercase (string<-list (each _.uppercase me)))
  (to _.capitalize (chain ((me .slice 0 1) .uppercase) (me .slice 1)))
  (to (_ .replace pattern replacement) ;TODO more efficient
    ;; TODO unify the cases?
    (hm (if pattern.none?
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
                (else (link (me i) (scanning i.+)))))))))
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
    ("" .join (each (-> me) (range<- n))))
  (to (_ .format @arguments)
    (string<-writer (-> (format .to-sink it me @arguments))))
  (to _.split-lines
    ;; TODO ugly. This 'if' is needed because we want a final "\n" to
    ;; yield the same output as a string with no final "\n". N.B. while
    ;; that's convenient it's also information-destroying.
    (let tweaked (if (and me.some? (= me.last #\newline))
                     (me .slice 0 me.count.-)
                     me))
    ;; TODO it'd be nice for efficiency if tweaked could be a view instead of a copy
    (tweaked .split "\n"))
  (to (_ .selfie sink)
    (sink .display #\")
    (for each! ((c me))
      (sink .display (may c            ;XXX super slow. We might prefer to use the Scheme built-in.
                       (be #\\ "\\\\")
                       (be #\" "\\\"")
                       (be #\newline "\\n")
                       (be #\tab     "\\t")
                       (be #\return  "\\r")
                       ;; XXX escape the control chars
                       (else c))))
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
  (to _.printable?    (<= 32 (__char->integer me) 126))  ; TODO better name?
  (to (_ .compare c)  (__char-compare me c))
  (to (_ .+ n)   ;; Is this a good idea?
    (surely (integer? n) "Bad arg type" n)
    (char<- (+ me.code n)))
  (to (_ .- b)
    (may b
      (be (? integer?) (char<- (- me.code b)))
      (be (? char?)    (- me.code b.code))
      (else (error "Bad arg type" b))))
  (to (_ .to< b)      (range<- me b))       ;These methods should be in a trait
  (to (_ .to b)       (range<- me b.+))    ;if they're a good idea at all...
  (to (_ .span n)     (range<- me (+ me n)))
  (to _.+             (me .+ 1))      ;experiment
  (to _.-             (me .- 1))      ;experiment
  )

;; TODO: should a box be a collection?
(make-trait box-primitive me
  (to _.^             (__box-value me))
  (to (_ .^= val)     (__box-value-set! me val))
  (to (_ .update f) ;TODO better name? I left out the '!' to emphasize it returns the value
    (let value (f me.^))
    (me .^= value)
    value)                              ;TODO return void instead?
  (to (_ .selfie sink)
    (sink .display "<box ")
    (sink .write me.^)
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
  (to (_ .display a)   (unless (__display a me) (me .write a))) ; TODO is this dangerous? longer-term, design the whole display/write thing differently
  (to (_ .write-u8 u8) (__put-u8 me u8))
  (to (_ .write a)     (a .selfie me))
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
    (sink .write me.tag)
    (for each! ((arg me.arguments))
      (sink .display " ")
      (sink .write arg))
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
  (to _.none?         #yes)
  (to _.first         (error "No more frames" me))
  (to _.rest          (error "No more frames" me))
  (to (_ .selfie sink)   (sink .display "<halt-cont>"))
  (to message (list-trait me message)))

(make-trait __cont-trait me   ;; For the non-halt cont types
  (to _.none?         #no)
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


;; Interpreter

(make squeam
  (to (_ .play exp env)
    (__evaluate (if (squeam .expression? exp)
                    exp
                    (squeam .parse-expression exp))
                env))
  (to (_ .expression? x)
    (__expression? x))
  (to (_ .parse-expression x @(optional context))
    (parse-exp x (or context '())))                      ;TODO rename
  )


;; Hash-maps
;; This is defined in the runtime, here, because the form
;; (export foo bar) gets expanded into code like
;;   (map<- `((foo ,foo) (bar ,bar))) 
;; (but hygienic, when I get to fixing the current bad hygiene).

;; TODO:
;;   extend map-trait
;;   test deletion more
;;   nonlinear probing -- now quadratic, but how about xor probing?
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
        (let keys  (box<- [none]))  ;; size a power of 2
        (let vals  (box<- [#no]))   ;; same size

       ;; temp performance tracking
;;       (let n-places (box<- 0))
;;       (let n-probes (box<- 0))

        (to (capacity) keys.^.count)

        (to (occupants)
          (begin walking ((i (- (capacity) 1)))
            (hm (if (< i 0)       '())
                (do (let k (keys.^ i)))
                (if (= k none)    (walking i.-))
                (if (= k deleted) (walking i.-))
                (else             (link i (walking i.-))))))

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
       
        (make map {extending map-trait}
          (to (_ key)
            (may (place key)
              (be {at i} (vals.^ i))
              (else      (error "Missing key" map key))))
          (to (_ .get key @(optional default))
            (may (place key)
              (be {at i} (vals.^ i))
              (else      default)))
          (to (_ .set! key val)
            (may (place key)
              (be {at i}
                (vals.^ .set! i val))
              (be {missing-at i}
                (keys.^ .set! i key)
                (vals.^ .set! i val)
                (count .^= count.^.+)
                (maybe-grow))))
          (to (_ .maps? key)
            (may (place key)
              (be {at _} #yes)
              (else      #no)))
          (to _.none? (= count.^ 0))
          (to _.count  count.^)
          (to _.keys   (each keys.^ (occupants))) ;XXX lazy-map
          (to _.values (each vals.^ (occupants)))
          (to _.items
            (let ks keys.^)
            (let vs vals.^)
            (for each ((i (occupants)))
              `(,(ks i) ,(vs i))))
          (to (_ .get-set! key value<-)
            (may (place key)
              (be {at i}
                (vals.^ i))
              (be {missing-at _}
                (let value (value<-))
                ;; Alas, we can't just stick it in at i because (value<-)
                ;; might have changed things too:
                (map .set! key value)
                value)))
          (to (_ .delete! key)
            (may (place key)
              (be {at i}
                (keys.^ .set! i deleted)
                (count .^= count.^.-)
                #no)
              (else #no)))   ;XXX error instead? It is in Python.
          (to (_ .find? value)
            (map.values .find? value))
          (to (_ .find value default)
            (let vs vals.^)
            (begin searching ((js (occupants)))  ;XXX should be lazy
              (hm (if js.none? default)
                  (if (= value (vs js.first)) (keys.^ js.first))
                  (else (searching js.rest)))))
          (to _.clear!
            (count .^= 0)
            (keys .^= [none])
            (vals .^= [#no]))
          (to _.copy
            (map<- map.items))
          (to (_ .update key f)         ;TODO define in a mutable-map-trait ?
            (let value (f (map key)))  ;TODO what about a (map .get key) version? how to factor this?
            (map .set! key value)
            value)
          (to (_ .selfie sink)
            (sink .display "#<map (")
            (sink .write count.^)
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
  (make set {extending map-trait}
    (to _.none?           map.none?)
    (to _.count            map.count)
    (to _.keys             map.keys)
    (to (_ .maps? key)     (map .maps? key))
    (to _.copy             (set<-list map.keys)) ;TODO tune
    (to (_ .add! key)      (map .set! key 1))    ;N.B. matching the bag type here
    (to (_ .add-all! vals) (for each! ((v vals)) (set .add! v)))
    (to (_ .union! other)  (set .add-all! other.keys))
    (to (_ .union other)
      (let result set.copy)
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
      (sink .write map.keys)
      (sink .display ">"))
    ))


;; stdlib

(to (surely ok? @arguments)
  (unless ok?
    (error @(if arguments.none? '("Assertion failed") arguments))))

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
      (may xs
        (be `()           #yes)
        (be `(,x1 ,@rest) (and (compare? x0 x1)
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
  (may x
    (be -1 #yes)
    (be  0 #yes)
    (be +1 #yes)
    (else  #no)))


;;XXX so should some of these be in list-trait?

(to (as-list seq)            ;XXX naming convention for coercions?
  (if (list? seq)
      seq
      (begin copying ((seq seq))
        (if seq.none?
            '()
            (link seq.first (copying seq.rest))))))

(to (string<-list chars) (__string<-list (as-list chars)))
(to (array<-list xs)     (__array<-list (as-list xs)))

(to (reverse xs)
  (for foldl ((result '()) (x xs))
    (link x result)))

(to (foldl f z xs)           ; 'z' connoting zero from f's perspective
  (if xs.none?
      z
      (foldl f (f z xs.first) xs.rest)))

(to (foldr f xs z) ;N.B. some other languages have a different argument order
  (if xs.none?
      z
      (f xs.first (foldr f xs.rest z))))

(to (foldr1 f xs)
  (let tail xs.rest)
  (if tail.none?
      xs.first
      (f xs.first (foldr1 f tail))))

(make each
  (to (_ f xs)
    (for foldr ((x xs) (results '()))
      (link (f x) results)))
  (to (_ f @lists)
    (for each ((args (transpose lists)))
      (f @args))))

(make each!
  (to (_ f xs)
    (unless xs.none?
      (f xs.first)
      (each! f xs.rest)))
  (to (_ f @lists)
    (for each! ((args (transpose lists)))
      (f @args))))

(make zip
  (to (_ xs ys)                           ;specialized for speed
    (to (mismatch)
      (error "zip: mismatched arguments" xs ys))
    (begin zipping ((xs xs) (ys ys))
      (hm (if xs.none? (if ys.none? '() (mismatch)))
          (if ys.none? (mismatch))
          (else `((,xs.first ,ys.first)
                  ,@(zipping xs.rest ys.rest))))))
  (to (_ @lists)
    (transpose lists)))

;; TODO: name it (zip @rows) instead, like Python?
(to (transpose rows)
  (if (every _.none? rows)   ; and make it (some _.none? rows)?
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
  ;; Inlining of (those identity (each maybe xs))
  (for foldr ((x xs) (kept '()))
    (may (maybe x)
      (be #no  kept)
      (be yeah (link yeah kept)))))

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
  (and xs.some?
       (or (pass? xs.first)
           (some pass? xs.rest))))

(to (every pass? xs)
  (or xs.none?
      (and (pass? xs.first)
           (every pass? xs.rest))))

(make range<-
  (to (_ first limit)
    (if (<= limit first)
        '()
        (make range {extending list-trait}
          (to _.none? #no)
          (to _.first first)
          (to _.rest  (range<- first.+ limit))
          (to _.count (- limit first))
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
                  (to _.none? #no)
                  (to _.first first)
                  (to _.rest  (range<- (+ first stride) limit stride))
                  (to (_ i)
                    (error "TODO" range `(,i)))
                  (to (_ .maps? i)
                    (error "TODO" range (_ .maps? i)))
                  )))
        (if (< stride 0)
            (if (< first limit)
                '()
                (make range {extending list-trait}
                  (to _.none? #no)
                  (to _.first first)
                  (to _.rest  (range<- (+ first stride) limit stride))
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
    (if xs.none?
        '()
        (make enumeration {extending list-trait}
          (to _.none? #no)
          (to _.first `(,i ,xs.first))
          (to _.rest  (enumerate xs.rest i.+))))))

(to (array<- @elements)
  (array<-list elements))

(to (string<- @chars)
  (string<-list chars))

(to (string<-writer take-sink)
  (let sink (string-sink<-))
  (take-sink sink)
  sink.output-string)


;; (Roughly) undo parse-exp and parse-pat.
;; Really we should track source-position info instead, and report that.
;; This is just to make debugging less painful till then.

(let (list<- unparse-exp unparse-pat unparse-clause)
  (hide

    (to (unparse-exp e)
      (may e.term
        (be {constant c}
          (if (self-evaluating? c) c `',c))
        (be {variable v}
          v)
        (be {make name stamp trait clauses}
          (unparse-make name stamp trait clauses))
        (be {do e1 e2}
          (unparse-do e1 e2))
        (be {let p e}
          `(let ,(unparse-pat p) ,(unparse-exp e)))
        (be {call e1 e2}
          (may e2.term
            (be {list operands}
              `(,(unparse-exp e1) ,@(each unparse-exp operands)))
            (be {term (? cue? cue) operands}
              `(,(unparse-exp e1) ,cue ,@(each unparse-exp operands)))
            (else
              `(call ,(unparse-exp e1) ,(unparse-exp e2)))))
        (be {term tag es}
          (term<- tag (each unparse-exp es)))
        (be {list es}
          `(list<- ,@(each unparse-exp es))))) ;XXX unhygienic

    (to (unparse-do e1 e2)
      (let es
        (begin unparsing ((tail e2))
          (may tail.term
            (be {do e3 e4} (link e3 (unparsing e4)))
            (else          `(,tail)))))
      `(do ,@(each unparse-exp (link e1 es))))

    (to (unparse-make name stamp trait-term clauses)
      (surely (= {constant #no} stamp.term)) ;XXX
      `(make ,name
         ,@(may trait-term.term
             (be {constant #no} '())
             (be trait-e        `({extending ,(unparse-exp trait-e)})))
         ,@(each unparse-clause clauses)))

    (to (unparse-clause `(,p ,p-vars ,e-vars ,e))
      `(to ,(unparse-pat p) ,(unparse-exp e)))

    (to (unparse-pat pat)
      ;; XXX these need updating to the newer pattern syntax
      (may pat.term
        (be {constant-pat c}
          (if (self-evaluating? c) c `',c))
        (be {any-pat}
          '_)
        (be {variable-pat v}
          v)
        (be {term-pat tag ps}
          (term<- tag (each unparse-pat ps)))
        (be {list-pat ps}
          (each unparse-pat ps))        ;TODO especially unacceptable now
        (be {and-pat p1 p2}
          `(<and-pat> ,(unparse-pat p1) ,(unparse-pat p2)))
        (be {view-pat e p}
          `(<view-pat> ,(unparse-exp e) ,(unparse-pat p)))))

    (list<- unparse-exp unparse-pat unparse-clause)))


;; printf-ish thing. TODO do something completely different?
(let format
  (hide

    (make format
      (to (_ format-string @arguments)
        (scanning out format-string.values arguments))
      (to (_ .to-sink sink format-string @arguments)
        (scanning sink format-string.values arguments)))

    ;;TODO actually design the format language

    (to (scanning sink s args)
      (if s.none? 
          (when args.some?
            (error "Leftover arguments to .format" args))
          (may s.first
            (be #\~
              (parse sink s.rest args))
            (be ch
              (sink .display ch)
              (scanning sink s.rest args)))))

    ;; Parse a ~ field, then go back to scanning the rest of s.
    (to (parse sink s args)
      (let {pair sign s1} (if (s .prefix? '(#\-))
                              {pair -1 s.rest}
                              {pair #no s}))
      (if (s1 .prefix? '(#\0))
          (parsing sink s1.rest #\0     sign 0   args)
          (parsing sink s1      #\space sign #no args)))

    (to (parsing sink s pad sign width args)
      (when s.none?
        (error "Incomplete format")) ;TODO report the format-string
      (may s.first
        (be #\w
          (maybe-pad sink pad sign width (_ .write args.first))
          (scanning sink s.rest args.rest))
        (be #\d
          (maybe-pad sink pad sign width (_ .display args.first))
          (scanning sink s.rest args.rest))
        (be #\~
          (sink .display "~") ;TODO: complain if there were formatting directives before "~"?
          (scanning sink s.rest args))
        (be (? _.digit? ch)
          (let new-width (+ (- ch #\0)
                            (if width (* 10 width) 0)))
          (parsing sink s.rest pad sign new-width args))
        (be #\x  ; hex number, XXX works wrong on negative numbers
          (maybe-pad sink pad sign width (_ .display ((string<-number args.first 16) .lowercase)))
          (scanning sink s.rest args.rest))
        (else
          (error "Bad format string" (string<-list s)))))

    (to (maybe-pad sink pad sign width message)
      (hm (when width
            ;; TODO fix: we're currently justifying to width, but not truncating
            (let w (if sign (* sign width) width))
            (sink .display ((string<-writer message) .justify w pad)))
          (when sign
            (error "Missing width in format string"))
          (else
            (call sink message))))

    format))
