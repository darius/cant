; Miasma: x86 Scheme Assembler.
; Copyright (C) 2000, Darius Bacon, Brian Spilsbury, Alycat
; Refer to legal/License.txt

(#x0F #x77 EMMS            "Empty MMX state")

(#x0F #x6E /r MOVD mm r/m32 "")
(#x0F #x7E /r MOVD r/m32 mm "")

(#x0F #x6F /r MOVQ mm mm/m64 "")
(#x0F #x7F /r MOVQ mm/m64 mm "")
