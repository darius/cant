(to (main `(,_ ,filename))
  (let elfcode (parse (with-input-file '.read-lines filename)))
  (itsy-dump elfcode)
  (newline)
  (disassemble elfcode))

(to (parse lines)
  (let ip-reg (number<-string (lines.first.split 1)))
  (let program (array<-list
                (for each ((line lines.rest))
                  (let v line.split)
                  `(,(symbol<- v.first) ,@(each number<-string v.rest)))))
  {elfcode ip-reg program})

(to (itsy-dump {elfcode ip-reg program})
  (format "enum { ip = ~w }\n" ip-reg)
  (format "enum { program_length = ~w }\n" program.count)
  (format "let program: [program_length] Insn = [\n")
  (each! print-insn program)
  (format "];\n")
  (newline))

(to (print-insn (list<- op a b c))
  (format "  [~w, ~w, ~w, ~w],\n" op a b c))

(to (disassemble {elfcode ip-reg program})

  (to (stmt<- (list<- op a b c))

    (to (blah c-op x y)
      (case ((and (commutative .maps? c-op)
                  (not= x y)
                  (= {r c} y))
             (blah c-op y x))
            ((and (commutative .maps? c-op)
                  (= {r c} x))
             ("~d ~d= ~d"
              .format (arg<- x) c-op (arg<- y)))
            (else
             ("~d = ~d ~d ~d"
              .format (arg<- {r c}) (arg<- x) c-op (arg<- y)))))

    (let commutative (set<- "+" "*" "&" "|"))

    (to (arg<- x)
      (match x
        ({r i} (if (= i ip-reg) "IP" ("r~w" .format i)))
        ({i n} (string<-number n))
        (#no   "")))

    (match op
      ('addr  (blah "+"  {r a} {r b}))
      ('addi  (blah "+"  {r a} {i b}))
      ('mulr  (blah "*"  {r a} {r b}))
      ('muli  (blah "*"  {r a} {i b}))
      ('banr  (blah "&"  {r a} {r b}))
      ('bani  (blah "&"  {r a} {i b}))
      ('borr  (blah "|"  {r a} {r b}))
      ('bori  (blah "|"  {r a} {i b}))
      ('setr  (blah ""   {r a} #no))
      ('seti  (blah ""   {i a} #no))
      ('gtir  (blah ">"  {i a} {r b}))
      ('gtri  (blah ">"  {r a} {i b}))
      ('gtrr  (blah ">"  {r a} {r b}))
      ('eqir  (blah "==" {i a} {r b}))
      ('eqri  (blah "==" {r a} {i b}))
      ('eqrr  (blah "==" {r a} {r b}))))

  (format "#ip ~w\n\n" ip-reg)
  (let stmts (each stmt<- program))
  (each! print stmts.items)
  'XXX)
