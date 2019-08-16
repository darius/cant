;; (Use run.scm to run this.)

(let input (with-input-file _.read-all data-file))

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
  (_.results (parson-parse parse-main string)))

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
    (to (_ .do op a b c)
      (let result
        (may op

          (be 'addr  (+ (regs a) (regs b)))
          (be 'addi  (+ (regs a) b))

          (be 'mulr  (* (regs a) (regs b)))
          (be 'muli  (* (regs a) b))

          (be 'banr  ((regs a) .and (regs b)))
          (be 'bani  ((regs a) .and b))
       
          (be 'borr  ((regs a) .or (regs b)))
          (be 'bori  ((regs a) .or b))
       
          (be 'setr  (regs a))
          (be 'seti  a)
       
          ;; TODO I'm not sure about this method name claim.count
          (be 'gtir  (_.count (> a (regs b))))
          (be 'gtri  (_.count (> (regs a) b)))
          (be 'gtrr  (_.count (> (regs a) (regs b))))
       
          (be 'eqir  (_.count (= a (regs b))))
          (be 'eqri  (_.count (= (regs a) b)))
          (be 'eqrr  (_.count (= (regs a) (regs b))))
       
          ))
      (regs .set! c result))

    (to _.get-regs regs)
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
  (for tally-by ((`(,_ ,compatible-ops) constraints))
    (<= 3 compatible-ops.count)))

(format "~w\n" (part-1))


(display "\nPart 2\n")

(to (part-2)
  ;; TODO: map-reduce with intersection
  (let op-name-set all-op-names.range)
  (let opcodes ((each _.first constraints) .range))
  (let candidates (map-by (-> op-name-set) opcodes.keys))
  (for each! ((`(,opcode ,op-names) constraints))
    (candidates .set! opcode
                ((candidates opcode) .intersect op-names.range)))

;; It turns out the above could've been just
;;  (let candidates
;;    (map<-lists (for each ((`(,opcode ,op-names) constraints))
;;                  `(,opcode ,op-names.range))))
;; even though `constraints` has multiple entries per opcode, sometimes
;; different ones. But the above could've been needed, depending on the data.

  ;; Trivial constraint satisfaction turns out to be good enough:
  (let assignments (map<-))
  (begin pruning ()
    (let opcode (min-by (compose _.count candidates)
                        candidates.keys))
    (when (= 1 (_.count (candidates opcode)))
      (let op-name (((candidates opcode) .keys) .first)) ;clumsy
      (assignments .set! opcode op-name)
      (candidates .delete! opcode)
      (for each! (((_ op2 names2) candidates.items))
        (names2 .delete! op-name))
      (when candidates.some?
        (pruning))))
  (surely candidates.none?)

  (each! print assignments.items)
  (newline)

  (let post-regs (run program assignments))
  (post-regs 0))

(format "~w\n" (part-2))
