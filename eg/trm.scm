;; Text Register Machine interpreter
;; Ported from github.com/darius/sketchbook (trm.py)
;; http://www.indiana.edu/~iulg/trm/
;; Glossary:
;;   pc    program counter
;;   insn  instruction
;;   n     argument part of instruction
;;   reg   register

(import (use "lib/parson") parse grammar<- feed)

(let loud? (box<- #no))

(to (trm-parse program)
  (vector<-list ((parse parser program) .opt-results)))

(let grammar (grammar<- "
program: insn* :end.
insn:    {'1'+} {'#' '#'? '#'? '#'? '#'?} :make_insn.
"))

(to (insn<- ones hashes)
  `(,(insn-table hashes.count) ,ones.count))

(let parser
  ((grammar (map<- `(("make_insn" ,(feed insn<-)))))
   "program"))

(to (regs<- @strings)
  (vector<-list `(#no ,@strings)))

(to (run insns regs @(optional loud?))
  (begin stepping ((pc 0))
    (when (< pc insns.count)
      (when loud?
        (show insns pc regs)
        (newline))
      (let (fn n) (insns pc))
      (let d (fn n regs))
      (surely (not= d 0))
      (stepping (+ pc d))))
  regs)

(let insn-table
  (vector<-
   'illegal-insn
   (make _
     ({.name} "add-1")
     ((n regs)
      (regs .set! n (chain (regs n) "1"))
      1))
   (make _
     ({.name} "add-#")
     ((n regs)
      (regs .set! n (chain (regs n) "#"))
      1))
   (make _
     ({.name} "forward")
     ((n regs) n))
   (make _
     ({.name} "backward")
     ((n regs) (- n)))
   (make _
     ({.name} "case")
     ((n regs)
      (match (regs .get n "")           ;TODO how about just (regs n)?
        ("" 1)
        (str (regs .set! n str.rest)
             (match str.first
               (#\1 2)
               (#\# 3))))))))

;; (import (use "lib/pretty-layout") ...)
;;XXX use me

(make show
  ((insns)
   (show insns 0 (regs<-)))             ;TODO fancier (optional ...)
  ((insns pc regs)
   (let left
     (for each (((addr (fn n)) insns.items))
       (let show-addr (if (= addr pc)
                          "   "
                          ("~3w" .format (abs (- pc addr)))))
       ("~d ~d ~w" .format show-addr fn.name n)))
   (let right (for each (((i str) regs.items.rest))
                ("\tr~w: ~d" .format i str)))
   (for each! ((line (abut left right)))
     (format "~d\n" line))))

(to (abut lines1 lines2)
  (flip (chain (flip lines1) (flip lines2))))

(to (flip strings)
  (for each ((row (transpose-padded strings #\space)))
    (call string<- row)))

(to (transpose-padded lists padding)
  (begin zipping ((lists lists))
    (if (every '.empty? lists)
        '()
        `(,(for each ((list lists))
             (if list.empty? padding list.first))
          ,@(zipping (for each ((list lists))
                       (if list.empty? '() list.rest)))))))

;; Smoke test
(to (main _)
  (show (trm-parse "1#111##"))
  (newline)

  (let move-r2-r1 (trm-parse "11#####111111###111###1##1111####1#111111####"))
  (let regs (regs<- "" "1#1#11##"))
  (show move-r2-r1 0 regs)
  (run move-r2-r1 regs)
  (print regs.rest))


(export trm-parse regs<- run show)
