(import (use "lib/text-wrap") wrap)

(let text "
  How to explain?  How to describe?  Even the omniscient viewpoint fails.
")

(each! print (wrap text 30))
