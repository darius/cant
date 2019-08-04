(import (use "eg/miasma/registers") register?)

;; Instruction specs

(to (spec<- mnemonic stem params+ doc-string uses)
  (let params (for those ((p params+))
                (not= p '{bytes u 0 0})))
  (make spec                            ;TODO struct maker?
    ({.mnemonic} mnemonic)
    ({.stem} stem)
    ({.params} params)
    ({.doc-string} doc-string)
    ({.uses} uses)
    ({.unparse}
     `(,mnemonic
       ,@(each unparse-param params)
       ,doc-string))))

(to (unparse-param param)
  param)                      ;TODO did I never write this originally?

;; Return an unambiguous mnemonic, given the name of an overloaded one
;; and the params that resolve the overloading.
(to (mnemonic<- stem params)
  (combined-mnemonic<- stem (each coerce-string (suffixes params))))

;; Return a string that looks like X, an atom.
(to (coerce-string x)
  ("~d" .format x))

;; Return an instruction mnemonic formed out of STEM and SPECS.
(to (combined-mnemonic<- stem specs)
  (symbol<- ("-" .join `(,stem ,@specs)))) ;TODO join with some other separator? "/"?

(to (suffixes x)
  (be x
    ((? symbol?) (if ('(Sreg cr dr =16 =32 + /r /0 /1 /2 /3 /4 /5 /6 /7) .find? x)
                     '()
                     `(,x)))
    ((? list?)   (gather suffixes x))
    (_           '())))

;; A map of all specs by fully-qualified mnemonic.
(let the-specs (map<-))

;; Load the specs from the i386 instruction table.
(to (setup-spec-table)
  the-specs.clear!
  (load-table (with-input-file read-all "eg/miasma/tables/i386.scm")))

(to (load-table sexprs)
  (for each! ((sexpr sexprs))
    (let spec (parse-spec sexpr))
    (surely (not (the-specs .maps? spec.mnemonic)))
    (the-specs .set! spec.mnemonic spec)))

;; Return the spec with mnemonic MNEMONIC.
(to (find-spec mnemonic)
  (the-specs mnemonic))


;; Parse instruction table entries.

(to (parse-spec sexpr)
  (surely (and (list? sexpr)
               (<= 3 sexpr.count)
               (symbol? sexpr.first))
          "Spec has the basics")
  (let stem sexpr.first.name)
  ;; We copy sexpr.rest until we hit the doc-string.
  ;; Preceding were our params, and the reg/flag list optionally follows.
  (let `(,params (,doc-string ,@(optional uses)))
    (split-on string? sexpr.rest))
  (spec<- (mnemonic<- stem params)
          stem
          (each parse-param params)
          doc-string
          uses))

;; Is x an unsigned byte?
(to (byte? x)
  (and (count? x) (<= x 255)))

;; TODO This might make a good use case for nested-list Parson.
(to (parse-param param)
  (be (expand-abbrev param)
    ((? byte? b)
     (opcode-byte-param b))
    ((? register? r)
     (register-param r))
    ('=16
     (size-mode-param 16))
    ('=32
     (size-mode-param 16))
    ((? operand? operand)
     (parse-operand operand))
    (`(,first ,@rest)
     (let args (each expand-abbrev rest))
     (let L args.count)
     (be `(,first ,@args)
       (`(? ,(? byte? b))
        (condition-param b))
       (`(+ ,(? byte? b) ,(? (operand-of 'G) operand)) ;TODO
        (opcode+register-param b operand))
       (`(/r ,arg1 ,arg2)
        (hm (if (and ((operand-of 'E) arg1) ((operand-of 'G) arg2))
                (Ex.Gx-param arg1 arg2))
            (if (and ((operand-of 'G) arg1) ((operand-of 'E) arg2))
                (Gx.Ex-param arg1 arg2))))
       (`(,foo ,arg)
        (surely (extended-opcode-tags .find? foo))
        (surely ((operand-of 'E) arg))
        (let extended-opcode (extended-opcode-tags .find foo))
        (Ex-param extended-opcode arg))))))

;; Return a symbol whose name is the concatenation of ATOMS.
(to (concat-symbol @atoms)
  (symbol<- ("" .join (each coerce-string atoms))))

;;XXX finish porting
(let abbrevs
  (for each ((`(,a ,b) (grid* '(E G I U M R J O S) '(b w v d)))
             (`(,x ,y) (grid* '(E G I U E E J O S) '(1 2 4 4))))
    `(,(concat-symbol a b) ,x ,y)))

; XXX: preserve semantics of M, R, v

(to (expand-abbrev x)
  (or (assoc x abbrevs) x))

(let extended-opcode-tags '#(/0 /1 /2 /3 /4 /5 /6 /7))

(to (operand? x)
  (be x
    (`(,(? symbol?) ,(? symbol? tag) ,size)
     (and ('(I U J O) .find? tag)
          ('(1 2 4) .find? size)))
    (_ #no)))

(to ((operand-of tag) x)                ;XXX duplicate code
  (be x
    (`(,(? symbol?) ,(? (-> (= it tag))) ,size)
     ('(1 2 4) .find? size))
    (_ #no)))

(to (parse-operand `(,symbol ,tag ,size))
  (be tag
    ('I (signed-immediate-param size))
    ('U (unsigned-immediate-param size))
    ('J (relative-jump-param size))
    ('O (offset-param size))))

;; TODO name like foo-param<-

;; A literal opcode byte to output as is.
(to (opcode-byte-param byte)
  `{bytes u 1 ,byte})             ;TODO use lists instead? or objects?

;; A literal register whose identity is implicit in the opcode.
(to (register-param register)
  `{bytes u 0 0})

;; There's a global assumption in this assembler that we're in 32-bit
;; mode. So for 32 bits we do nothing, and for 16 bits we emit an
;; operand-size prefix.
(to (size-mode-param bits)
  (if (= bits 32)
      `{bytes u 0 0}
      `{bytes u 1 0x66}))

;; An immediate field as specified by an operand.
(to (signed-immediate-param size)
  `{bytes i ,size {arg int}})

;; The other type of immediate field.
(to (unsigned-immediate-param size)
  `{bytes u ,size {arg int}})

;; A pc-relative jump offset.
(to (relative-jump-param size)
  `{bytes i ,size {op - {arg int} {hereafter}}})

;; A segment-relative offset field (if I understand this... probably not)
(to (offset-param size)
  `{bytes u ,size {arg int}})

;; A condition code field. In Intel syntax, condition codes are part
;; of the mnemonic, but in my syntax they're a separate argument. The
;; encoding of the condition gets added to OPCODE-BYTE on emission.
(to (condition-param opcode-byte)
  `{bytes u 1 {op + ,opcode-byte {arg cc}}})

;; A general-register field that's added to an extended-opcode byte.
;FIXME: confusing name
(to (opcode+register-param opcode-byte `(,symbol ,tag ,size))
  `{bytes u 1 {op + ,opcode-byte {arg reg ,size}}})

;; A pair of fields that go into a mod-r/m encoding. Ex is effective
;; address, Gx is general register.
(to (Ex.Gx-param Ex `(,Gx-symbol ,tag ,size))
  `{swap-args {mod-r/m {arg reg ,size} {arg ,Ex}}})

;; Like Ex.Gx, but with source arguments in the opposite order.
(to (Gx.Ex-param `(,Gx-symbol ,tag ,size) Ex)
  `{mod-r/m {arg reg ,size} {arg ,Ex}})

;; Like Ex.Gx, this becomes a mod-r/m, but with 3 extended opcode bits
;; in place of the general register code.
(to (Ex-param extended-opcode operand)
  `{mod-r/m ,extended-opcode {arg ,operand}})

(export the-specs setup-spec-table)
