typedef   signed int   i32;     /* 32-bit, signed */
typedef unsigned int   u32;     /* 32-bit, unsigned */
typedef   signed short i16;     /* 16-bit, signed */
typedef unsigned short u16;     /* 16-bit, unsigned */
typedef   signed char   i8;     /*  8-bit, signed */
typedef unsigned char   u8;     /*  8-bit, unsigned */

typedef u8 *x86_bufptr;

#define x86_pushing(ptr, x)  (x86_tmp = (x), *(ptr) = x86_tmp)

#define x86_push_u8(x)    ( x86_pushing(--x86_bptr,x) )
#define x86_push_u16(x)   ( x86_pushing((u16 *)(x86_bptr -= 2),x) )
#define x86_push_u32(x)   ( x86_pushing((u32 *)(x86_bptr -= 4),x) )

#define x86_push_i8(x)    ( x86_pushing(--x86_bptr,x) )
#define x86_push_i16(x)   ( x86_pushing((i16 *)(x86_bptr -= 2),x) )
#define x86_push_i32(x)   ( x86_pushing((i32 *)(x86_bptr -= 4),x) )

#define mod_rm(reg, mrm)  ( x86_push_u8 (((reg)<<3) | (mrm)) )
#define modrm(mod, rm)    ( ((mod)<<6) | (rm) )

#define at_reg(r)         ( modrm (0, (r)) )
#define at_mem(addr)      ( x86_push_i32 (addr), modrm (0, 5) )

#define atb(r, offset)    ( x86_push_i8 (offset), modrm (1, (r)) )

#define atv(r, offset)    ( x86_push_i32 (offset), modrm (2, (r)) )

#define reg(r)            ( modrm (3, (r)) )
