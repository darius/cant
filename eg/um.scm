;; Universal Machine from http://www.boundvariable.org/task.shtml
;; Ported from https://github.com/darius/superbench
;; ~/git/superbench/um/um.lua

;;XXX .u<op> means small, unsigned op
;; and I'm gonna assume 32-bit here
;; Similar for .s<op> (signed).
;; Do these mix OK? Is this reasonable?

;; TODO: mem could also be a typed array, but neither the array
;; nor the type is as simple...

(to (run program sink source)
  (let mem (flexarray<-))         ;TODO: shorter name? flexlist?
  (mem .push! program)
  (let free-list (flexarray<-))
  (let reg (array<-count 8 0))         ;TODO: typed array, for efficiency

  (begin running ((program program) (pc 0))

    (to (next)
      (running program (pc .u+ 1)))

    (let inst (program pc))
    (let opcode (inst .u>> 28))

    (match opcode
      (13 (reg .set! (7 .and (inst .u>> 25))
               (inst .and 0x1FFFFFF))
          (next))
      (_
       (let a (7 .and (inst .u>> 6)))
       (let b (7 .and (inst .u>> 3)))
       (let c (7 .and inst))
       (match opcode

         (0 (when (not= (reg c) 0)
              (reg .set! a (reg b)))
            (next))

         (1 (reg .set! a ((mem (reg b))
                          (reg c)))
            (next))

         (2 ((mem (reg a)) .set! (reg b) (reg c))
            (next))

         (3 (reg .set! a ((reg b) .s+ (reg c)))
            (next))

         (4 (reg .set! a ((reg b) .s* (reg c)))
            (next))

         (5 (reg .set! a ((reg b) .u/ (reg c)))
            (next))

         (6 (reg .set! a (((reg b) .and (reg c)) .not))
            (next))

         (7 "Successful exit")

         (8 (let chunk (array<-count (reg c) 0)) ;TODO: typed array again
            (reg .set! b
                 (case (free-list.empty?
                        (mem .push! chunk))
                       (else
                        (let i free-list.pop!)
                        (mem .set! i chunk)
                        i)))
            (next))

         (9 (mem .set! (reg c) #no)    ;XXX what was 'none' about again?
            (free-list .push! (reg c))
            (next))

         (10 (sink .write-char (char<- (reg c)))
             (next))

         (11 (let s source.read-char)
             (reg .set! c
                  (if (eof? s) 0xFFFFFFFF (string<- s)))
             (next))

         (12 (case ((= (reg b) 0)
                    (running program (reg c)))
                   (else
                    (let new-program ((mem (reg b)) .copy))
                    (mem .set! 0 new-program)
                    (running new-program (reg c)))))

         (_ (error "Bad opcode" opcode)))))))

(to (read-program source)
  (let program (flexarray<-))
  (begin reading ()
    (let c3 source.read-char)
    (unless (eof? c3)
      (let c2 source.read-char)
      (let c1 source.read-char)
      (let c0 source.read-char)
      ;; TODO: a syntax for int-guard coercion instead?
      (program .push!
               (u32<-bytes c3.code c2.code c1.code c0.code))
      (reading)))
  program)                              ;TODO: snapshot it

(to (u32<-bytes b3 b2 b1 b0)
  (append-byte (append-byte (append-byte b3 b2) b1) b0))

(to (append-byte u byte)
  (byte .u+ (u .u<< 8)))

(hide
 (to (testme)
   (let program (with-input-file read-program "sandmark.umz"))
   (run program out stdin))   
 (testme))
