squeam-binary: squeam.scm terp/util.scm terp/macros.scm terp/read.scm terp/parse.scm terp/env.scm terp/elaborate.scm terp/primitives.scm terp/terp.scm 
	echo '(compile-program "squeam.scm" "squeam-binary")' | scheme -q
