;; Universal Machine from http://www.boundvariable.org/task.shtml
;; Ported from https://github.com/darius/superbench
;; ~/git/superbench/um/um.lua

;;XXX .u<op> means small, unsigned op
;; and I'm gonna assume 32-bit here
;; Similar for .s<op> (signed).
;; Do these mix OK? Is this reasonable?

;; TODO: mem could also be a typed array, but neither the array
;; nor the type is as simple...

(to (trace @args)
  (call format args))

(let mask32 (- (expt 2 32) 1))

(to (run program sink source)
  (let mem (flexarray<-))
  (mem .push! program)
  (let free-list (flexarray<-))
  (let reg (array<-count 8 0))         ;TODO: typed array, for efficiency

  (let insn-count (box<- 0))
  (let tick-count (box<- 0))

  (begin running ((program program) (pc 0))
    
;    (insn-count .^= (+ insn-count.^ 1))
;    (insn-count .^= 0)
;    (tick-count .^= (+ tick-count.^ 1))
;    (when (= tick-count.^ 30000)
;      (os-exit 1))

    (to (next)
      (running program (pc .u+ 1)))

    (let inst (program pc))
    (let opcode (inst .u>> 28))

    '(hide
      (to (R i) (reg i))  ;; To print as unsigned like the reference output.
      (format "~08x [~4x ~4x ~4x ~4x ~4x ~4x ~4x ~4x] ~08x "
              pc
              (R 0) (R 1) (R 2) (R 3) (R 4) (R 5) (R 6) (R 7)
              inst))

    (match opcode
      (13 ;(trace "r~w = 0x~08x\n" (7 .and (inst .u>> 25)) (inst .and 0x1FFFFFF))
          (reg .set! (7 .and (inst .u>> 25))
               (inst .and 0x1FFFFFF))
          (next))
      (_
       (let a (7 .and (inst .u>> 6)))
       (let b (7 .and (inst .u>> 3)))
       (let c (7 .and inst))
       (match opcode

         (0 ;(trace "if (r~w) r~w = r~w\n" c a b)
            (unless (= (reg c) 0)
              (reg .set! a (reg b)))
            (next))

         (1 ;(trace "r~w = r~w[r~w]\n" a b c)
            (reg .set! a ((mem (reg b))
                          (reg c)))
            (next))

         (2 ;(trace "r~w[r~w] = r~w\n" a b c)
            ((mem (reg a)) .set! (reg b) (reg c))
            (next))

         (3 ;(trace "r~w = r~w + r~w\n" a b c)
            (reg .set! a ((reg b) .s+ (reg c)))
            (next))

         (4 ;(trace "r~w = r~w * r~w\n" a b c)
            (reg .set! a ((reg b) .s* (reg c)))
            (next))

         (5 ;(trace "r~w = r~w / r~w\n" a b c)
            (reg .set! a ((reg b) .u/ (reg c)))
            (next))

         (6 ;(trace "r~w = ~~(r~w & r~w)\n" a b c)
            ;; TODO these mask32 .and's shouldn't be needed with a proper `word32` type
            (reg .set! a (mask32 .and (((reg b) .and (reg c)) .not)))
            (next))

         (7 ;(trace "halt\n")
            "Successful exit")

         (8 ;(trace "r~w = allocate r~w\n" b c)
            (let chunk (array<-count (reg c) 0)) ;TODO: typed array again
            (reg .set! b
                 (case (free-list.empty?
                        (mem .push! chunk))
                       (else
                        (let i free-list.pop!)
                        (mem .set! i chunk)
                        i)))
            (next))

         (9 ;(trace "abandon r~w\n" c)
            (mem .set! (reg c) #no)    ;XXX what was 'none' about again?
            (free-list .push! (reg c))
            (next))

         (10 ;(trace "output r~w\n" c)
             (sink .display (char<- (reg c)))
             (next))

         (11 ;(trace "r~w = input\n" c)  ;; N.B. C version has fflush (stdout); first
             (let s source.read-char)
             (reg .set! c
                  (if (eof? s) mask32 (string<- s))) ;XXX shouldn't this be an array of bytes?
             (next))

         (12 ;(trace "dup r~w; finger r~w\n" b c)
             (case ((= (reg b) 0)
                    (running program (reg c)))
                   (else
                    (let new-program (mem (reg b)))  ; originally ((mem (reg b)) .copy)) -- why?
                    (mem .set! 0 new-program)
                    (running new-program (reg c)))))

         (_ (error "Bad opcode" opcode)))))))

(to (read-program source)
  (let program (flexarray<-))
  (begin reading ()
    (let b3 source.read-u8)
    (unless (eof? b3)
      (let b2 source.read-u8)
      (let b1 source.read-u8)
      (let b0 source.read-u8)
      ;; TODO: a syntax for int-guard coercion instead?
      (program .push!
               (u32<-bytes b3 b2 b1 b0))
      (reading)))
  program) ;.snapshot)

(to (u32<-bytes b3 b2 b1 b0)
  (append-byte (append-byte (append-byte b3 b2) b1) b0))

(to (append-byte u byte)
  (byte .u+ (u .u<< 8)))

;; XXX put something like this in stdlib
(to (with-binary-input-file fn filename)
  (let source (open-binary-file-source filename))
  (let result (fn source))
  source.close                       ;TODO unwind-protect
  result)

(to (with-binary-output-file fn filename)
  (let source (open-binary-file-sink filename))
  (let result (fn source))
  source.close                       ;TODO unwind-protect
  result)

(to (main `(,_ ,filename))
  (let program (with-binary-input-file read-program filename))
  (run program out stdin))
