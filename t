#!/bin/bash
set -euo pipefail; shopt -s failglob  # 'Bash strict mode'

# Test runner. Usage: t foo.scm

time scheme --script $1 >$1.out
diff -u $1.expected $1.out
