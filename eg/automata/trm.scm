;; Text Register Machine interpreter
;; Ported from github.com/darius/sketchbook (trm.py)
;; http://www.indiana.edu/~iulg/trm/
;; Glossary:
;;   pc    program counter
;;   insn  instruction
;;   n     argument part of instruction
;;   reg   register

(import (use 'parson) parse grammar<- feed)

(to (trm-parse program)
  (array<-list ((parse parser program) .results)))

(let grammar (grammar<- "
program: insn* :end.
insn:    {'1'+} {'#' '#'? '#'? '#'? '#'?} :make_insn.
"))

(to (insn<- ones hashes)
  {insn (insn-table hashes.count) ones.count})

(let parser
  ((grammar (map<- `((make_insn ,(feed insn<-)))))
   'program))

(to (regs<- @strings)
  (array<- #no @strings))

(to (run insns regs @(optional loud?))
  (begin stepping ((pc 0))
    (when (insns .maps? pc)
      (when loud?
        (show insns pc regs)
        (newline))
      (let {insn fn n} (insns pc))
      (let d (fn n regs))
      (surely (not= d 0))
      (stepping (+ pc d))))
  regs)

(let insn-table
  (array<-
   'illegal-insn
   (make _
     (to _.name "add-1")
     (to (_ n regs)
       (regs .set! n (chain (regs n) "1"))
       1))
   (make _
     (to _.name "add-#")
     (to (_ n regs)
       (regs .set! n (chain (regs n) "#"))
       1))
   (make _
     (to _.name "forward")
     (to (_ n regs) n))
   (make _
     (to _.name "backward")
     (to (_ n regs) (- n)))
   (make _
     (to _.name "case")
     (to (_ n regs)
       (may (regs .get n "")           ;TODO how about just (regs n)?
         (be "" 1)
         (be str
           (regs .set! n str.rest)
           (may str.first
             (be #\1 2)
             (be #\# 3))))))))

;; (import (use 'pretty-layout) ...)
;;XXX use me

(to (show insns @(optional ?pc ?regs))
  (let pc   (or ?pc   0))
  (let regs (or ?regs (regs<-)))
  (let left
    (for each ((`(,addr ,{insn fn n}) insns.items))
      (let show-addr (if (= addr pc)
                         "   "
                         ("~3w" .format (abs (- pc addr)))))
      ("~d ~d ~w" .format show-addr fn.name n)))
  (let right (for each ((`(,i ,str) regs.items.rest))
               ("\tr~w: ~d" .format i str)))
  (for each! ((line (abut left right)))
    (format "~d\n" line)))

(to (abut lines1 lines2)
  (flip (chain (flip lines1)
               (flip lines2))))

(to (flip strings)
  (each string<-list (transpose-padded strings #\space)))

(to (transpose-padded lists padding)
  (begin zipping ((lists lists))
    (if (every _.none? lists)
        '()
        (link (each (_ .get 0 padding) lists)
              (zipping (each (_ .slice 1) lists))))))

;; Smoke test
(to (main _)
  (show (trm-parse "1#111##"))
  (newline)

  (let move-r2-r1 (trm-parse "11#####111111###111###1##1111####1#111111####"))
  (let regs (regs<- "" "1#1#11##"))
  (show move-r2-r1 0 regs)
  (run move-r2-r1 regs)
  (print regs.rest))


(export trm-parse regs<- run show main)
