;; Universal Machine from http://www.boundvariable.org/task.shtml
;; Ported from https://github.com/darius/superbench
;; ~/git/superbench/um/um.lua
;; (using not-yet-implemented features and syntax)

;; TODO: s/vector/array?

(import io)                             ;XXX these convey capabilities
(import os)

(define (run program)
  (let mem (growable-vector<-))         ;TODO: shorter name? flexlist?
  (.append! mem program)
  (let free-list (growable-vector<-))
  (let reg (vector<-count 8 0))         ;TODO: typed vector, for efficiency
  ;; TODO: mem could also be a typed vector, but neither the vector
  ;; nor the type is as simple...
  (recurse running ((program program) (pc 0))
    (let inst (program pc))
    (let opcode (.u>> inst 28))
    ;;XXX .u<op> means small, unsigned op
    ;; and I'm gonna assume 32-bit here
    ;; Similar for .s<op> (signed).
    ;; Do these mix OK? Is this reasonable?

    (match opcode
      (13 (.set! reg (.bit-and 7 (.u>> inst 25))
                 (.bit-and inst 0x1FFFFFF))
          (running program (.u+ pc 1)))
      (_
       (let a (.bit-and 7 (.u>> inst 6)))
       (let b (.bit-and 7 (.u>> inst 3)))
       (let c (.bit-and 7 inst))
       (match opcode

         (0 (when (not= (reg c) 0)
              (.set! reg a (reg b)))
            (running program (.u+ pc 1)))

         (1 (.set! reg a
                   ((mem (reg b)) (reg c)))
            (running program (.u+ pc 1)))

         (2 (.set! (mem (reg a)) (reg b)
                   (reg c))
            (running program (.u+ pc 1)))

         (3 (.set! reg a
                   (.s+ (reg b) (reg c)))
            (running program (.u+ pc 1)))

         (4 (.set! reg a
                   (.s* (reg b) (reg c)))
            (running program (.u+ pc 1)))

         (5 (.set! reg a
                   (.u/ (reg b) (reg c)))
            (running program (.u+ pc 1)))

         (6 (.set! reg a
                   (.bit-not (.bit-and (reg b) (reg c))))
            (running program (.u+ pc 1)))

         (7 (os:exit 0))

         (8 (let chunk (vector<-count (reg c) 0)) ;TODO: typed vector again
            (.set! reg b
                   (cond ((.empty? free-list)
                          ;; TODO: mem could have a method doing this?
                          (let t (.count mem))
                          (.append! mem chunk)
                          t)
                         (else
                          (let t (.pop! free-list))
                          (.set! mem t chunk)
                          t)))
            (running program (.u+ pc 1)))

         (9 (.set! mem (reg c) none)
            (.append! free-list (reg c))
            (running program (.u+ pc 1)))

         (10 (.write-char io:stdout (char<- (reg c)))
             (running program (.u+ pc 1)))

         (11 (let s (.read-char io:stdin))
             (.set! reg c
                    (if (is? s none) 0xFFFFFFFF (string<- s)))
             (running program (.u+ pc 1)))

         (12 (cond ((= (reg b) 0)
                    (running program (reg c)))
                   (else
                    (let new-program (.shallow-copy (mem (reg b))))
                    (.set! mem 0 new-program)
                    (running new-program (reg c)))))

         (_ (.writeln io:stdout "Bad opcode")
            (os:exit 1)))))))

(define (read-program filename)
  (let program (growable-vector<-))
  (io:call-with-input-file filename "rb"
    (given (f)
      (recurse reading ()
        (let c0 (.read-char f))
        (when (not (is? c0 none))
          (let c1 (.read-char f))
          (let c2 (.read-char f))
          (let c3 (.read-char f))
          (.append! program
                    (.u+ (.u<< (.u+ (.u<< (.u+ (.u<< c0 8)
                                               c1)
                                          8)
                                    c2)
                               8)
                         c3))
          (reading)))))
  program)
