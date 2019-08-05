;; (Use run.scm to run this.)

(let input (with-input-file _.read-lines data-file))

(let the-ip (number<-string (input.first.split 1)))
(print the-ip)

(let the-program (array<-list
                  (for each ((line input.rest))
                    (let v line.split)
                    `(,(symbol<- v.first) ,@(each number<-string v.rest)))))
(each! print the-program)

(to (show insn)
  (call "~d ~w ~w ~w" `{.format ,@insn}))

(to (vm<- regs ip program)
  (make vm

    (to _.run
      (begin running ()
        (when vm.step (running))))
;       (when (program .maps? (regs ip))
;         (format "ip=~w ~w ~d " (regs ip) regs (show (program (regs ip))))
 ;        vm.step
 ;        (format " ~w\n" regs)
  ;       (running))))

    (to _.step
      (let here (regs ip))
      (be (program .get here)
        (#no #no)
        (`(,op ,a ,b ,c)
         (vm .do op a b c)
         (regs .set! ip (+ (regs ip) 1))
         (when (= here 7)
           (format "ip=~w ~w ~d " (regs ip) regs (show (program (regs ip)))))
         #yes)))

    (to (_ .do op a b c)
      (let result
        (be op

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
       
          ;; TODO I'm not sure about this method name claim.count
          ('gtir  (_.count (> a (regs b))))
          ('gtri  (_.count (> (regs a) b)))
          ('gtrr  (_.count (> (regs a) (regs b))))
       
          ('eqir  (_.count (= a (regs b))))
          ('eqri  (_.count (= (regs a) b)))
          ('eqrr  (_.count (= (regs a) (regs b))))

          ))
      (regs .set! c result))

    (to _.get-regs
      regs)
    ))

(display "\nPart 1\n")

(let the-regs (array<-count 6 0))
(let the-vm (vm<- the-regs the-ip the-program))

(to (part-1)
  the-vm.run
  (the-regs 0))

;(format "~w\n" (part-1))


(display "\nPart 2\n")

(to (part-2)
  (the-regs .set! 0 1)
  the-vm.run
  (the-regs 0))

(format "~w\n" (part-2))
