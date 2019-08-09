;; https://en.wikipedia.org/wiki/Brainfuck
;; Doing it two ways: interpreter and compiler.
;; The input programs must have balanced brackets.
;; See also http://rosettacode.org/wiki/Execute_Brain****

;; Interpreter

(to (bf-interpret program)
  (let data (map<-))                    ; run-time data store
  (let jump (match-brackets program))   ; jump targets
  (begin running ((i 0)                 ; instruction pointer
                  (d 0))                ; data pointer
    (may (program .get i)
      (be #\[ (running (if (= 0 (data .get d 0)) (jump i) i.+)
                       d))
      (be #\] (running (if (= 0 (data .get d 0)) i.+ (jump i))
                       d))
      (be #no 'done)
      (be ch  (running i.+
                       (may ch
                         (be #\< d.-)
                         (be #\> d.+)
                         (else (may ch
                                 (be #\- (data .set! d (- (data .get d 0) 1)))
                                 (be #\+ (data .set! d (+ (data .get d 0) 1)))
                                 (be #\. (display (char<- (data .get d 0))))
                                 (be #\, (data .set! d (read-1 stdin)))
                                 (else))
                               d)))))))

(to (match-brackets program)
  (let jump (map<-))
  (for foldl ((stack '()) (`(,i ,ch) program.items))
    (may ch
      (be #\[ (link i stack))
      (be #\] (jump .set! stack.first i.+)
              (jump .set! i stack.first.+)
              stack.rest)
      (else   stack)))
  jump)

(to (read-1 source)
  (may source.read-char
    (be (? eof?) -1)
    (be ch ch.code)))


;; Compiler
;; One semantic difference from the interpreter:
;; bounded memory (using an array instead of a hashmap).
;; You know, since this compiler is about as short as the interpreter
;; there's not much point to the former's existence...

(to (bf-compile program)
  (let meaningful (set<-list "<>-+.,[]"))
  (let real-program (for those ((ch program.values))
                      (meaningful .maps? ch)))
  (let expr-stack
    (for foldl ((stack '(0)) (ch real-program))
      (may ch
        (be #\[ (link 'd stack))
        (be #\] (let `(,top ,next ,@rest) stack)
                (link `(begin looping ((d ,next))
                         (if (= 0 (data d))
                             d
                             (looping ,top)))
                      rest))
        (else (let `(,top ,@rest) stack)
              (link (may ch
                      (be #\< `(- ,top 1))
                      (be #\> `(+ ,top 1))
                      (be #\- `(decr ,top))
                      (be #\+ `(incr ,top))
                      (be #\. `(emit ,top))
                      (be #\, `(absorb ,top)))
                    rest)))))
  (surely (= 1 expr-stack.count))
  (bf-complete-program expr-stack.first))

(to (bf-complete-program body)
  `(on ()
     (let data (array<-count 30000 0))
     (to (decr d)
       (data .update d _.-)
       d)
     (to (incr d)
       (data .update d _.+)
       d)
     (to (emit d)
       (display (char<- (data d)))
       d)
     (to (absorb d)
       (data .set! d (read-1 stdin))
       d)
     ,body))


;; Smoke test

(to (main _)
  (let text (with-input-file _.read-all "eg/brainfuck/hello.bf"))
  (bf-interpret text)
  (let code (bf-compile text))
;;  (print code)
  ((evaluate code '()))
  )


(export bf-interpret bf-compile)
