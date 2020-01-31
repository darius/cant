; Miasma: x86 Scheme Assembler.
; Copyright (C) 2000, Darius Bacon, Brian Spilsbury, Alycat
; Refer to legal/License.txt

;; floating-point instructions

(#xD9 #xF0 F2XM1           "Replace ST(0) with 2^ST(0) - 1")
(#xD9 #xE1 FABS            "Replace ST with its absolute value")

;(#xD8 /0   FADD m32 real    "fp add")
;(#xDC /0   FADD m64 real    "fp add")
;(#xD8 /0   FADD m64 real    "fp add")



;; m32real m64real m80real
;; manual page 34

;; ST or ST(0)

;; ST(i)

;; m16int m32int m64int

;; mm
;; (mmx)

;; mm/m32
;; mm/m64

