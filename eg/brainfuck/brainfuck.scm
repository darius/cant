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
    (be (program .get i)
      (#\[ (running (if (= 0 (data .get d 0))
                        (jump i)
                        (+ i 1))
                    d))
      (#\] (running (if (= 0 (data .get d 0))
                        (+ i 1)
                        (jump i))
                    d))
      (#no 'done)
      (ch  (running (+ i 1)
                    (be ch
                      (#\< (- d 1))
                      (#\> (+ d 1))
                      (_ (be ch
                           (#\- (data .set! d (- (data .get d 0) 1)))
                           (#\+ (data .set! d (+ (data .get d 0) 1)))
                           (#\. (display (char<- (data .get d 0))))
                           (#\, (data .set! d (be stdin.read-char
                                                ((? eof?) -1)
                                                (ch ch.code))))
                           (_))
                         d)))))))

(to (match-brackets program)
  (let jump (map<-))
  (for foldl ((stack '()) (`(,i ,ch) program.items))
    (be ch
      (#\[ (link i stack))
      (#\] (jump .set! stack.first (+ i 1))
           (jump .set! i (+ stack.first 1))
           stack.rest)
      (_   stack)))
  jump)


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
      (be ch
        (#\[ (link 'd stack))
        (#\] (let `(,top ,next ,@rest) stack)
             (link `(begin looping ((d ,next))
                      (if (= 0 (data d))
                          d
                          (looping ,top)))
                   rest))
        (_ (let `(,top ,@rest) stack)
           (link (be ch
                   (#\< `(- ,top 1))
                   (#\> `(+ ,top 1))
                   (#\- `(decr ,top))
                   (#\+ `(incr ,top))
                   (#\. `(emit ,top))
                   (#\, `(absorb ,top)))
                 rest)))))
  (surely (= 1 expr-stack.count))
  (bf-complete-program expr-stack.first))

(to (bf-complete-program body)
  `(on ()
     (let data (array<-count 30000 0))
     (to (decr d)
       (data .set! d (- (data d) 1))
       d)
     (to (incr d)
       (data .set! d (+ (data d) 1))
       d)
     (to (emit d)
       (display (char<- (data d)))
       d)
     (to (absorb d)
       (data .set! d (be stdin.read-char
                       ((? eof?) -1)
                       (ch ch.code)))
       d)
     ,body))


;; Smoke test

(to (main _)
  (let text (with-input-file '.read-all "eg/brainfuck/hello.bf"))
  (bf-interpret text)
  (let code (bf-compile text))
;;  (print code)
  ((evaluate code '()))
  )


(export bf-interpret bf-compile)
