; Miasma: x86 Scheme Assembler.
; Copyright (C) 2000, Darius Bacon, Brian Spilsbury, Alycat
; Refer to legal/License.txt

; FIXME: this is 486 and above only.  Separate it out.
;(#xC8 +    BSWAP r32     "Reverse the byte order of a register")




(#x0F #x08    INVD    "Invalide internal caches")

(#x0F #x01 /7 INVLPG m    "Invalide TLB entry")

(#x0F #x09    WBINVD    "Write back and invalidate cache")

(#x0F #xC0 /r XADD r/m8  r8  "Exchange and add")
(#x0F #xC1 /r XADD r/m32 r32 "Exchange and add")


