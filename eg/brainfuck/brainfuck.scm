;; https://en.wikipedia.org/wiki/Brainfuck

(to (bf-interpret program)

  (let data (map<-))                    ; run-time data store
  (let jump (match-brackets program))   ; jump targets

  (begin running ((i 0)                 ; instruction pointer
                  (d 0))                ; data pointer
    (match (program .get i)
      (#\> (running (+ i 1) (+ d 1)))
      (#\< (running (+ i 1) (- d 1)))
      (#\+ (data .set! d (+ (data .get d 0) 1))
           (running (+ i 1) d))
      (#\- (data .set! d (- (data .get d 0) 1))
           (running (+ i 1) d))
      (#\. (display (char<- (data .get d 0)))
           (running (+ i 1) d))
      (#\, (data .set! d (match stdin.read-char
                           ((? eof?) -1)
                           (ch ch.code)))
           (running (+ i 1) d))
      (#\[ (running (if (= 0 (data .get d 0))
                        (jump i)
                        (+ i 1))
                    d))
      (#\] (running (if (not= 0 (data .get d 0))
                        (jump i)
                        (+ i 1))
                    d))
      (#no 'done)
      (_   (running (+ i 1) d)))))

(to (match-brackets program)
  (let jump (map<-))
  (for foldl ((stack '()) (`(,i ,ch) program.items))
    (match ch
      (#\[ (link i stack))
      (#\] (jump .set! stack.first (+ i 1))
           (jump .set! i (+ stack.first 1))
           stack.rest)
      (_   stack)))
  jump)

;; smoke test

(to (main _)
  (bf-interpret (with-input-file '.read-all "eg/brainfuck/hello.bf")))
