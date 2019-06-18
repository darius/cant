;; OK, let's get an idea what search-squeam-code should look like,
;; from an actual application: finding instances of the pattern syntax
;; I'm about to outlaw. I.e., patterns like (a b) instead of `(,a ,b).

(import (use "lib/pretty-print") pp)
(import (use "lib/squeam-source-walker")
  expr-subparts patt-subparts macroexpand-outer-patt)

(to (main `(,_ ,@filenames))
  (each! report-badness filenames))

(to (report-badness filename)
;;  (format "Checking ~d...\n" filename)
  (match (those bad-expr? (with-input-file read-all filename))
    ('() 'ok)
    (top-level-bad-exprs
     (format "In ~d these top-level expressions harbor badness:\n" filename)
     (each! pp top-level-bad-exprs)
     (newline))))

(to (bad-expr? expr)
  (bad-part? (expr-subparts expr)))

(to (bad-part? `(,subexprs ,subpatts))
  (or (some bad-expr? subexprs)
      (some bad-patt? subpatts)))

(to (bad-patt? patt)
  (let p (macroexpand-outer-patt patt))
  (match p
    (`(,s ,@_)
     (if (not ('(list<- link quote and view @ quasiquote) .find? s))
         (do (format "This subpattern is bad: ~w\n" patt)
             #yes)
         (bad-part? (patt-subparts p))))
    (_ (bad-part? (patt-subparts p)))))

(export main bad-expr? bad-patt?)
