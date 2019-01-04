Miasma generates code to emit x86 machine code, using tables of instructions.
This is a port of https://github.com/darius/miasma from Scheme.

Provided are emitters to C code and Python code, though I don't expect
to maintain them. If I end up using this it'll probably be as part of
a JIT compiler for Squeam in Squeam. In the meantime, it's just more
example code to help hammer out the Squeam language design.
