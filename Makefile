# A failed attempt to make a faster-starting binary. TODO revisit this

squeam-binary: squeam.scm player/util.scm player/macros.scm player/read.scm player/parse.scm player/env.scm player/elaborate.scm player/primitives.scm player/terp.scm 
	echo '(compile-program "squeam.scm" "squeam-binary")' | scheme -q
