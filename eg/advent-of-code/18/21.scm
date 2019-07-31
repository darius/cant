;; (Use run.scm to run this.)

(let input (with-input-file '.read-lines data-file))

(let the-ip (number<-string (input.first.split 1)))
(print the-ip)

(let the-program (array<-list
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
       
         ; TODO I'm not sure about this method name claim.count
         ('gtir  ('.count (> a (regs b))))
         ('gtri  ('.count (> (regs a) b)))
         ('gtrr  ('.count (> (regs a) (regs b))))
       
         ('eqir  ('.count (= a (regs b))))
         ('eqri  ('.count (= (regs a) b)))
         ('eqrr  ('.count (= (regs a) (regs b))))
       
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
