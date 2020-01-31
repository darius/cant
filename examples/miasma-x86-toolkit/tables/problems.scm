; Miasma: x86 Scheme Assembler.
; Copyright (C) 2000, Darius Bacon, Brian Spilsbury, Alycat
; Refer to legal/License.txt

; eax/ax register 
(#xA8      TEST %al    imm8  "Logical compare")
(#xA9      TEST %eax   imm32 "Logical compare")

(#x9A      CALL ptr16:32   "Call far, absolute")
(#xFF /3   CALL m16:32     "Call far, absolute indirect")

(#xEA      JMP ptr16:32  "Jump far, absolute, address given in operand")
(#xFF /5   JMP m16:32    "Jump far, absolute, address given in m16:32")

(#xA0      MOV %al      moffs8  "Move byte at (seg:offset) to AL")
(#xA1      MOV %eax     moffs32 "Move doubleword at (seg:offset) to EAX")
(#xA2      MOV moffs8  %al      "Move AL to (seg:offset)")
(#xA3      MOV moffs32 %eax     "Move EAX to (seg:offset)")

; theory: m32 always means r/m32, but with an overloading
; superseding the r.  (and Intel isn't consistent about it)
(#x8F /0   POP m32         "Pop from stack")
(#x58 +    POP r32         "Pop from stack")


; 16-bit mode or 32-bit mode?
(#x63 /r   ARPL r/m16 r16  "Adjust RPL of 1st to not less than RPL of 2nd")

; wtf is this?
(#x62 /r   BOUND r32 m32&32 "Bounds-checking")

(#xC5 /r      LDS r32 m16:32 "Load DS and r32 with far pointer from memory")
(#xC4 /r      LES r32 m16:32 "Load ES and r32 with far pointer from memory")
(#x0F #xB2 /r LSS r32 m16:32 "Load SS and r32 with far pointer from memory")
(#x0F #xB4 /r LFS r32 m16:32 "Load FS and r32 with far pointer from memory")
(#x0F #xB5 /r LGS r32 m16:32 "Load GS and r32 with far pointer from memory")

(#x0F #x01 /2 LGDT m16&32 "Load global descriptor table")
(#x0F #x01 /3 LIDT m16&32 "Load interrupt descriptor table")

(#x0F #x01 /0 SGDT m      "Store global descriptor table")
(#x0F #x01 /1 SIDT m      "Store interrupt descriptor table")

(#x0F #x00 /2 LLDT r/m16  "Load local descriptor table")
; ahem! above is inconsistent with below
(#x0F #x00 /0 SLDT r/m32  "Store local descriptor table")

(#x0F #x01 /6 LMSW r/m16  "Load machine status word")

(#x0F #x01 /4 SMSW r/m16  "Store machine status word")
(#x0F #x01 /4 SMSW r32/m16  "Store machine status word")
; the above r32/m16 is exactly what the manual says.  ugh!
; Consider just ignoring it...

(#x0F #x00 /3 LTR r/m16     "Load task register")

(#x0F #x00 /1 STR r/m16     "Store task register")

; the manual doesn't mention /r -- I assume an oversight.
(#x0F #xA4 /r SHLD r/m32 r32 imm8 "Double precision shift left")
(#x0F #xA5 /r SHLD r/m32 r32 %cl  "Double precision shift left")
; note 3-argument instructions above

(#x0F #xAC /r SHRD r/m32 r32 imm8 "Double precision shift right")
(#x0F #xAD /r SHRD r/m32 r32 %cl  "Double precision shift right")
; note 3-argument instructions above

(#x0F #x00 /4 VERR r/m16 "Verify segment for reading")
(#x0F #x00 /5 VERR r/m16 "Verify segment for writing")
