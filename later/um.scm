;; Universal Machine from http://www.boundvariable.org/task.shtml
;; Ported from https://github.com/darius/superbench
;; ~/git/superbench/um/um.lua
;; (using not-yet-implemented features and syntax)

;; TODO: s/vector/array?

(define (run program out-port in-port)
  (let mem (growable-vector<-))         ;TODO: shorter name? flexlist?
  (.append! mem program)
  (let free-list (growable-vector<-))
  (let reg (vector<-count 8 0))         ;TODO: typed vector, for efficiency
  ;; TODO: mem could also be a typed vector, but neither the vector
  ;; nor the type is as simple...
  (recurse running ((program program) (pc 0))

    (define (next)
      (running program (.u+ pc 1)))

    (let inst (program pc))
    (let opcode (.u>> inst 28))
    ;;XXX .u<op> means small, unsigned op
    ;; and I'm gonna assume 32-bit here
    ;; Similar for .s<op> (signed).
    ;; Do these mix OK? Is this reasonable?

    (match opcode
      (13 (.set! reg (.bit-and 7 (.u>> inst 25))
                 (.bit-and inst #x1FFFFFF))
          (next))
      (_
       (let a (.bit-and 7 (.u>> inst 6)))
       (let b (.bit-and 7 (.u>> inst 3)))
       (let c (.bit-and 7 inst))
       (match opcode

         (0 (when (not= (reg c) 0)
              (.set! reg a (reg b)))
            (next))

         (1 (.set! reg a
                   ((mem (reg b)) (reg c)))
            (next))

         (2 (.set! (mem (reg a)) (reg b)
                   (reg c))
            (next))

         (3 (.set! reg a
                   (.s+ (reg b) (reg c)))
            (next))

         (4 (.set! reg a
                   (.s* (reg b) (reg c)))
            (next))

         (5 (.set! reg a
                   (.u/ (reg b) (reg c)))
            (next))

         (6 (.set! reg a
                   (.bit-not (.bit-and (reg b) (reg c))))
            (next))

         (7 "Successful exit")

         (8 (let chunk (vector<-count (reg c) 0)) ;TODO: typed vector again
            (.set! reg b
                   (cond ((.empty? free-list)
                          ;; TODO: mem could have a method doing this?
                          (let i (.count mem))
                          (.append! mem chunk)
                          i)
                         (else
                          (let i (.pop! free-list))
                          (.set! mem i chunk)
                          i)))
            (next))

         (9 (.set! mem (reg c) none)
            (.append! free-list (reg c))
            (next))

         (10 (.write-char out-port (char<- (reg c)))
             (next))

         (11 (let s (.read-char in-port))
             (.set! reg c
                    (if (is? s none) #xFFFFFFFF (string<- s)))
             (next))

         (12 (cond ((= (reg b) 0)
                    (running program (reg c)))
                   (else
                    (let new-program (.shallow-copy (mem (reg b))))
                    (.set! mem 0 new-program)
                    (running new-program (reg c)))))

         (_ (.writeln out-port "Bad opcode")
            "Error exit"))))))

(define (read-program in-port)
  (let program (growable-vector<-))
  (recurse reading ()
    (let c3 (.read-char in-port))
    (when (not (is? c3 none))
      (let c2 (.read-char in-port))
      (let c1 (.read-char in-port))
      (let c0 (.read-char in-port))
      ;; TODO: a syntax for int-guard coercion instead?
      (.append! program
                (u32<-bytes (.code c3) (.code c2) (.code c1) (.code c0)))
      (reading)))))
  program)                              ;TODO: snapshot it

(define (u32<-bytes b3 b2 b1 b0)
  (append-byte (append-byte (append-byte b3 b2) b1) b0))

(define (append-byte u byte)
  (.u+ (.u<< u 8) byte))
