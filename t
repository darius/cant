#!/bin/bash
set -euo pipefail; shopt -s failglob  # 'Bash strict mode'

# Test runner. Usage: for test/test-foo.scm, say `t foo`

time ./squeam.scm test/test-$1.scm >$1.out
diff -u $1.expected $1.out
