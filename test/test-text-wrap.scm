(import (use 'text-wrap) wrap)

(let text "
  How to explain?  How to describe?  Even the omniscient viewpoint quails.
")

(each! print (wrap text 30))
