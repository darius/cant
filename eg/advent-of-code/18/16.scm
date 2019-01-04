;; (Use run.scm to run this.)

(let input (with-input-file '.read-all data-file))

(let grammar (grammar<- "
main: clause* :hug '\n\n' program.  # N.B. no :end
clause: 'Before: [' [:nat ', ' :nat ', ' :nat ', ' :nat :hug] ']\n'
        [:nat ' ' :nat ' ' :nat ' ' :nat :hug] '\n'
        'After:  [' [:nat ', ' :nat ', ' :nat ', ' :nat :hug] ']\n\n' :hug.

program: insn* :hug.
insn:    :nat ' ' :nat ' ' :nat ' ' :nat '\n' :hug.
"))
(let semantics (grammar (map<-)))
(let parse-main (semantics 'main))
(to (parse string)
  ('.results (parson-parse parse-main string)))

(let `(,observations ,program) (parse input))
;(each! print program)
(format "#obs: ~w\n" observations.count)


(display "\nPart 1\n")

(to (run program assignments)
  (let regs (array<-count 4 0))
  (let vm (vm<- regs))
  (for each! ((`(,opcode ,a ,b ,c) program))
    (vm .do (assignments opcode) a b c))
  regs)

(to (vm<- regs)
  (make vm
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

(let all-op-names
  '(addr addi mulr muli
    banr bani borr bori
    setr seti
    gtir gtri gtrr eqir eqri eqrr))

(let constraints
  (for each ((sample observations))
    (let `(,regs-pre (,op ,a ,b ,c) ,regs-post) sample)
    (let compatible-ops
      (for those ((op-name all-op-names))
        (let vm (vm<- (array<-list regs-pre)))
        (vm .do op-name a b c)
        (<=> regs-post vm.get-regs)))
;;    (format "~w: ~w\n" op (sort compatible-ops))
    `(,op ,compatible-ops)))

(to (part-1)
  (for tally ((`(,_ ,compatible-ops) constraints))
    (<= 3 compatible-ops.count)))

(format "~w\n" (part-1))


(display "\nPart 2\n")

(to (part-2)
  ;; TODO: map-reduce with intersection
  (let op-name-set all-op-names.range)
  (let opcodes ((each '.first constraints) .range))
  (let candidates (map<- (for each ((opcode opcodes.keys))
                           `(,opcode ,op-name-set))))
  (for each! ((`(,opcode ,op-names) constraints))
    (candidates .set! opcode
                ((candidates opcode) .intersect op-names.range)))

;; It turns out the above could've been just
;;  (let candidates
;;    (map<- (for each ((`(,opcode ,op-names) constraints))
;;             `(,opcode ,op-names.range))))
;; even though `constraints` has multiple entries per opcode, sometimes
;; different ones. But the above could've been needed, depending on the data.

  ;; Trivial constraint satisfaction turns out to be good enough:
  (let assignments (map<-))
  (begin pruning ()
    (let opcode (min-by (compose '.count candidates)
                        candidates.keys))
    (when (= 1 ('.count (candidates opcode)))
      (let op-name (((candidates opcode) .keys) .first)) ;clumsy
      (assignments .set! opcode op-name)
      (candidates .delete! opcode)
      (for each! ((`(,op2 ,names2) candidates.items))
        (names2 .delete! op-name))
      (unless candidates.empty?
        (pruning))))
  (surely candidates.empty?)

  (each! print assignments.items)
  (newline)

  (let post-regs (run program assignments))
  (post-regs 0))

(format "~w\n" (part-2))
