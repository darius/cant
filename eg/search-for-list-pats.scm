;; OK, let's get an idea what search-squeam-code should look like,
;; from an actual application: finding instances of the pattern syntax
;; I'm about to outlaw. I.e., patterns like (a b) instead of `(,a ,b).

(import (use "lib/pretty-print") pp)
(import (use "lib/squeam-source-walker") expr-subparts patt-subparts)

(to (main `(,_ ,@filenames))
  (each! report-badness filenames))

(to (report-badness filename)
  (match (those bad-expr? (with-input-file read-all filename))
    ('() 'ok)
    (top-level-bad-exprs
     (format "In ~d these top-level expressions contain badness:\n" filename)
     (each! pp top-level-bad-exprs)
     (newline))))

(to (bad-expr? expr)
  (bad-part? (expr-subparts expr)))

(to (bad-part? `(,subexprs ,subpatts))
  (or (some bad-expr? subexprs)
      (some bad-patt? subpatts)))

(to (bad-patt? patt)
  (or (and (cons? patt)
           (not ('(quote quasiquote : @ optional) .find? patt.first)))
      (bad-part? (patt-subparts patt))))
