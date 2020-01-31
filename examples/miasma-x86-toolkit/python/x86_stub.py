import array

buf = array.array('B')

def freeze_code_buffer():
    global buf
    buf.reverse()
    code = buf.tostring()
    buf = array.array('B')
    return code

def push_u8(x):
    global buf
    buf.append(x)

def push_u16(x):
    push_u8((x & 0xFFFF) >> 8)
    push_u8(x & 0xFF)
    return x

def push_u32(x):
    push_u16((x & 0xFFFFFFFFL) >> 16)
    push_u16(x & 0xFFFF)
    return x

def push_i8(x):
    push_u8(x & 0xFF)
    return x

def push_i16(x):
    push_u16(x & 0xFFFF)
    return x

def push_i32(x):
    push_u32(x & 0xFFFFFFFFL)
    return x

def mod_rm(reg, mrm):
    push_u8((reg << 3) | mrm)

def modrm(mod, rm):
    return (mod << 6) | rm

def at_reg(r):
    return modrm(0, r)

def at_mem(addr):
    push_i32(addr)
    return modrm(0, 5)

def atb(r, offset):
    push_i8(offset)
    return modrm(1, r)

def atv(r, offset):
    push_i32(offset)
    return modrm(2, r)

def reg(r):
    return modrm(3, r)
