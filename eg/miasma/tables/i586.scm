; Miasma: x86 Scheme Assembler.
; Copyright (C) 2000, Darius Bacon, Brian Spilsbury, Alycat
; Refer to legal/License.txt

; rdmsr

; rdpmc (p5-mmx, p6)

(#x0F #x31 RDTSC           "Read time stamp counter")

(#x0F #x30 WRMSR           "Write to model specific register")


