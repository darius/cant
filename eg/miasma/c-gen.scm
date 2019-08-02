(import (use "eg/miasma/registers") register-number)
(import (use "eg/miasma/parse") the-specs setup-spec-table)
(import (use "eg/miasma/walk") walk-code walk-exp unit bind swapping eating)

(to (main _)
  (setup-spec-table)
  (generate-c-assembler "asm.h"))

(to (generate-c-assembler filename)
  (with-output-file c-write-assembler filename))

;; XXX duplicate code
(to (say-to sink @arguments)
  (for each! ((x arguments))
    (sink .display x))
  (sink .display "\n"))

(to (copy-file source sink)
  (sink .display source.read-all))

(to (c-write-assembler sink)
  (say-to sink "/* Generated by Miasma */")
  (say-to sink)
  (for with-input-file ((source "eg/miasma/c/asm_stuff.h"))
    (copy-file source sink))
  (say-to sink)
  (say-to sink (c-enum (sort register-number.items)))
  (say-to sink)
  (for each! ((mnemonic (sort the-specs.keys)))
    (let spec (the-specs mnemonic))
    (say-to sink (c-gen mnemonic spec.params))))


;; Code translation

(to (c-gen mnemonic code-list)
  (let vars (c-make-variable-list (sum (each c-variable-count code-list))))
  (c-stmt-macro (c-insn-name mnemonic)
                vars
                (c-body (each c-parenthesize vars) code-list)))

(to (c-make-variable-list n)
  (for each ((k (1 .to n)))
    ("v~w" .format k)))

(to (c-body vars code-list)
  (begin walking ((code-list code-list) (stmts '()) (vars vars))
    (match code-list
      (`() stmts)
      (`(,first ,@rest)
       ((walk-code first c-code c-exp)
        vars
        (given (vars2 cv) 
          (walking rest `(,cv ,@stmts) vars2)))))))

;; TODO walker objects instead, with code/exp methods?

(to (c-code code)
  (match code
    ({bytes signed? count exp}
     (for bind ((cv exp))
       (unit 
        (c-exp-stmt (c-call ("x86_push_~d~w" .format (if signed? "i" "u")
                                                     (* 8 count))
                            cv)))))
    ({swap-args code}
     (swapping code))
    ({mod-r/m e1 e2}
     (for bind ((cv1 e1))
       (for bind ((cv2 e2))
         (unit (c-exp-stmt (c-call "mod_rm" cv1 cv2))))))))

(to (c-exp exp)
  (match exp
    ({literal n}
     (unit (c-int-literal n)))
    ({op operator e1 e2}
     (for bind ((cv1 e1))
       (for bind ((cv2 e2))
         (unit (c-binop operator.name cv1 cv2)))))
    ({hereafter}
     (unit "hereafter"))
    ({arg @_}
     (eating unit))))


;; Variables

(to (c-variable-count code)

  (to (c-code code)
    (match code
      ({bytes _ _ exp}
       exp)
      ({swap-args code}
       code)
      ({mod-r/m e1 e2}
       (for bind ((cv1 e1))
         (for bind ((cv2 e2))
           (unit (+ cv1 cv2)))))))

  (to (c-exp exp)
    (match exp
      ({literal _}
       (unit 0))
      ({op operator e1 e2}
       (for bind ((cv1 e1))
         (for bind ((cv2 e2))
           (unit (+ cv1 cv2)))))
      ({hereafter}
       (unit 0))
      ({arg @_}
       (unit 1))))

  ((walk-code code c-code c-exp) '_
                                 (given (_ count) count)))


;; C code constructors

(to (c-enum items)
  (call chain                           ;XXX a little clumsy
        `("enum {\n"
          ,@(for each ((`(,sym ,val) items))
              ("  ~d = ~d,\n" .format (as-legal-c-identifier sym.name)
                                      (c-int-literal val)))
          "};")))

(to (c-int-literal n)
  (if (count? n)
      ("0x~d" .format (string<-number n 16)) ;TODO ~h format or something
      (string<-number n)))

(to (c-binop operator cv1 cv2)
  ("(~d ~d ~d)" .format cv1 operator cv2))

(to (c-parenthesize cv)
  (chain "(" cv ")"))

(to (c-call fn-cv @args-cv)
  ("~d(~d)" .format fn-cv (", " .join args-cv)))

(to (c-exp-stmt cv)
  (chain cv ";"))

(to (c-stmt-macro name vars stmts)
  (" \\\n" .join `(,(c-declare name vars) 
                   "  do {"
                   "    const u8 *hereafter = x86_bptr;"
                   ,@(for each ((stmt stmts))
                       (chain "    " stmt))
                   "  } while (0)")))

(to (c-declare name vars)
  ("#define ~d(~d)" .format name (", " .join vars)))

(to (c-insn-name mnemonic)
  (chain "x86_" (as-legal-c-identifier mnemonic.name)))

;; Return `str`, but munging out any characters that are used in our 
;; mnemonics but aren't legal in C identifiers.
(to (as-legal-c-identifier str)
  (string<-list (for yeahs ((ch str))
                  (and (not (":%" .find? ch))
                       (case (("-." .find? ch) #\_)
                             ((= #\? ch) #\c)
                             (else ch))))))
