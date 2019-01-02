;; From lib/stdlib.scm but currently unused. To consider some more.

(to (remove xs unwanted) ;TODO different arg order? N.B. almost unused
  (for those ((x xs))
    (not= x unwanted)))

