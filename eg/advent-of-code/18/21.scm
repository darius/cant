(let input (with-input-file '.read-lines "eg/advent-of-code/18/data/advent21"))

(let the-ip (number<-string (input.first.split 1)))
(print the-ip)

(let the-program (call array<-
                       (for each ((line input.rest))
                         (let v line.split)
                         `(,(symbol<- v.first) ,@(each number<-string v.rest)))))
(to (print-insn (list<- op a b c))
  (format "[~w,~w,~w,~w],\n" op a b c))

;(each! print-insn the-program)
;(print the-program.count)
;(exit 0)

(to (show insn)
  (call "~d ~w ~w ~w" `{.format ,@insn}))

(to (vm<- regs ip program)
  (make vm

    ({.run}
     (begin running ()
;       (when vm.step (running))))
       (when (program .maps? (regs ip))
         (format "ip=~w ~w ~d " (regs ip) regs (show (program (regs ip))))
         vm.step
         (format " ~w\n" regs)
        (running))))

    ({.step}
     (let here (regs ip))
     (match (program .get here)
       (#no #no)
       (`(,op ,a ,b ,c)
        (vm .do op a b c)
        (regs .set! ip (+ (regs ip) 1))
        (when (= here 7)
          (format "ip=~w ~w ~d " (regs ip) regs (show (program (regs ip)))))
        #yes)))

    ({.do op a b c}
     (let result
       (match op

         ('addr  (+ (regs a) (regs b)))
         ('addi  (+ (regs a) b))

         ('mulr  (* (regs a) (regs b)))
         ('muli  (* (regs a) b))

         ('banr  ((regs a) .and (regs b)))
         ('bani  ((regs a) .and b))
       
         ('borr  ((regs a) .or (regs b)))
         ('bori  ((regs a) .or b))
       
         ('setr  (regs a))
         ('seti  a)
       
         ('gtir  (if (> a (regs b)) 1 0))
         ('gtri  (if (> (regs a) b) 1 0))
         ('gtrr  (if (> (regs a) (regs b)) 1 0))
       
         ('eqir  (if (= a (regs b)) 1 0))
         ('eqri  (if (= (regs a) b) 1 0))
         ('eqrr  (if (= (regs a) (regs b)) 1 0))
       
         ))
     (regs .set! c result))

    ({.get-regs} regs)
    ))

(display "\nPart 1\n")

(let the-regs (array<-count 6 0))
(let the-vm (vm<- the-regs the-ip the-program))

(to (part-1)
  the-vm.run
  (the-regs 0))

(format "~w\n" (part-1))


(display "\nPart 2\n")

(to (part-2)
  'xxx)

;(format "~w\n" (part-2))
