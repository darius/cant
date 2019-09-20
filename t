#!/bin/bash
set -euo pipefail; shopt -s failglob  # 'Bash strict mode'

# Test runner. Usage: for test/test-foo.scm, say `t foo`

# XXX what is this crap? 
# make -s &&
# time ./cant-binary test/test-$1.cant >$1.out
# Exception: compiled program requires different compilation instance of (terp terp) from one found in terp/terp.scm

time ./incant test/test-$1.cant >$1.out
diff -u $1.expected $1.out
